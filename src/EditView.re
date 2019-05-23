open Belt;
open Common;
open Store;

type editState('a) = 
  | Edit('a)
  | Modified('a)
  | Saving('a)
  | Invalid('a, Js.Dict.t(Js.Dict.t(string)))
  | SaveFail('a, option(string), option(Js.Dict.t(Js.Dict.t(string))))
  | SaveSuccess('a);

type state = editState(Js.Json.t);

let getValue = event => ReactEvent.Form.target(event)##value;

[@react.component]
let make = (~resource: resource, ~id: string, ~entity:Js.Json.t, ~cancelAction )=> {

  let (state, setState) = React.useState(() => Edit(entity));

  let initialEntity = entity |> Js.Json.decodeObject |> Belt.Option.getExn;

  let getInitialFieldValue = (field:field):option(Js.Json.t) => Js.Dict.get(initialEntity, field.name);

  let validateField = (field:field, value: string):option(Js.Dict.t(string)) => {
    Belt.Option.flatMap(field.validators, validators => {
      let errors: Js.Dict.t(string) = Js.Dict.empty();
      Belt.Array.forEach(validators, validator => {

        switch(validator) {

          | `required => if (Js.String.length(Js.String.trim(value))== 0) {
            Js.Dict.set(errors, "required", "required");
          }

          | `email => {
            if (Js.String.length(Js.String.trim(value))== 0) {
              let emailRegex = [%bs.re {|/.*@.*\..+/|}];
              if (!value->Js.Re.test(emailRegex)){
                Js.Dict.set(errors, "email", "Email does not fit format: [name]@[host].[domain]");
              }
            }
          }
          | `name => {
            if (Js.String.length(Js.String.trim(value))== 0) {
              Js.Dict.set(errors, "name", "May not be empty");
            } else {
              let nameRegex = [%bs.re {|/^[\w\s]+$/|}];
              if (!value->Js.Re.test(nameRegex)){
                Js.Dict.set(errors, "name", "Name must only include alphanumeric characters and spaces");
              }
            }
          }
        }
      });
      switch(Belt.Array.size(Js.Dict.keys(errors))){
        | 0 => None
        | _ => Some(errors)
      };
    });
  };

  let validateEntity = (updatedEntity) : option(Js.Dict.t(Js.Dict.t(string))) => {
    let errors: Js.Dict.t(Js.Dict.t(string)) = Js.Dict.empty();

    Belt.Array.forEach(resource.fields, field => {
      switch(Js.Dict.get(updatedEntity, field.name)) {
        | Some(value) => {
          let rawVal = Store.Decode.singleFieldDecode(value, field);
          switch(validateField(field, rawVal)){
            | Some(fieldErrorDict) => Js.Dict.set(errors, field.name, fieldErrorDict)
            | None => () 
          }
        }
        | None => ()
      };
    });  

    switch(Belt.Array.size(Js.Dict.keys(errors))){
      | 0 => None
      | _ => Some(errors)
    };
  };

  let getUpdatedEntity = () : Js.Dict.t(Js.Json.t) => switch(state) {
    | Invalid(x,_)
    | Modified(x) => x |> Js.Json.decodeObject |> Belt.Option.getExn
    | _ => {
      // If not yet modified, then create a new entity to track the updated entity state.
      // NOTE: apparently "initialEntity" is still tied to "entity", so it must be copied over here...
      let currentCopy = Js.Dict.empty();
      initialEntity |> Js.Dict.entries |> Array.forEach(_,((key,v)) => Js.Dict.set(currentCopy, key, v));
      currentCopy
    } 
  };

  let getCurrentFieldValue = (field:field):option(Js.Json.t) => Js.Dict.get(getUpdatedEntity(), field.name);

  let isFieldModified = (field:field) => getCurrentFieldValue(field) != getInitialFieldValue(field);

  let updateField = (field:field, event) => {

    let value = Js.String.trim(event->getValue);
    
    let updatedEntity = getUpdatedEntity();

    // Track errors
    let currentErrors: Js.Dict.t(Js.Dict.t(string)) = switch(state) {
      | Invalid(_, errDict) => errDict
      | _ => Js.Dict.empty()
    };
    
    try(
      {
        Js.Dict.set(updatedEntity, field.name, Store.Encode.encodeField(field, value));
        switch(validateEntity(updatedEntity)){
          | Some(reportedErrors) => setState(_=> Invalid(updatedEntity |> Js.Json.object_, reportedErrors));
          | None => setState(_=> Modified(updatedEntity |> Js.Json.object_))
        };
      }
    ) {
      | Js.Exn.Error(e) =>
        let fieldErrorDict: Js.Dict.t(string) = Js.Dict.empty();
        switch (Js.Exn.message(e)) {
        | Some(message) => {
            Js.Dict.set(fieldErrorDict, "Js.Exn", message);
            Js.Dict.set(currentErrors, field.name, fieldErrorDict);
            setState(_=>Invalid(updatedEntity|> Js.Json.object_, currentErrors));
          }
        | None => Js.log2("An unknown error occurred on field: ", field.name);
        }
      | x => {
        let fieldErrorDict: Js.Dict.t(string) = Js.Dict.empty();
        Js.Dict.set(fieldErrorDict, "General Exception", {j|$x|j});
        Js.Dict.set(currentErrors, field.name, fieldErrorDict);
        setState(_=>Invalid(updatedEntity|> Js.Json.object_, currentErrors));
      }
    }; 

  };

  let getFieldError = (field: field) : option(string) => switch(state) {
      | SaveFail(_,optErrMsg, optCurrErrs) =>
      {
        switch(optCurrErrs) {
          | Some(currErrs) => {
            switch(Js.Dict.get(currErrs, field.name)){
              | Some(fieldErrs) => Some(
                Belt.Array.map(Js.Dict.entries(fieldErrs), ((key,msg)) => {j|$key: $msg|j})
                |> Js.Array.joinWith("\n"))
              | None => None
            }
          }
          | None => None
        }
      }       
      | Invalid(_,currErrs) =>{
        switch(Js.Dict.get(currErrs, field.name)){
          | Some(fieldErrs) => Some(
            Belt.Array.map(Js.Dict.entries(fieldErrs), ((key,msg)) => {j|$key: $msg|j})
            |> Js.Array.joinWith("\n"))
          | None => None
        }
      } 
      | _ => None
    };

  let getHtmlFieldType = (field) => switch(field.data_type) {
      | String => "text"
      | Integer => "number"
      | _ => "text"
    };

  let printRow = (field: field) => {

    let value = Belt.Option.mapWithDefault(
      getCurrentFieldValue(field), "", 
      (jsonValue) => Store.Decode.singleFieldDecode(jsonValue, field));
    
    <div key=("row-field-" ++ field.name) className="detail_table_row">
      <div className="md:text-right">
        <label className=("font-bold text-right" ++ (isFieldModified(field)?" bg-orange":"")) 
          htmlFor=("inline-" ++ field.name) >
          (str(field.title ++ ": "))
        </label>
      </div>
      ( if(field.editable){
        <input className="bg-gray-200 appearance-none border-2 border-gray-200 rounded max-w-md w-full py-1 leading-tight" 
          id=("inline-" ++ field.name) type_=getHtmlFieldType(field) 
          value=value 
          onChange=updateField(field)>
        </input>
      } else {
        <div id=("inline-" ++ field.name) type_=getHtmlFieldType(field)>(str(value))</div>
      })
      (switch(getFieldError(field)){
        | Some(err) => <span className="text-red">(str(err))</span>
        | None => ReasonReact.null
      })
    </div>
  };

  let printEntity = () => {

    <form className="detail_table" id="entity_table" >

      (resource.fields
      |> Array.map(_, field => {
        printRow(field);
      })
      |> ReasonReact.array)
    </form>
  };

  let printAsJson = entity => {
      (str(entity |> Js.Json.stringify))
  };

  let save = (currentEntity, _evt) => {
    setState(_=>Saving(currentEntity));

    Js.log("Save entity...");
    
    ApiClient.patchEntity(resource.name, id, currentEntity)
    |> Js.Promise.then_(result => {
      switch(result) {
        | Result.Ok(entity) => {
          Js.log2("Save success: ", entity);
          setState(_ => SaveSuccess(entity))
        }
        | Result.Error(message) =>{
          try ({

            // Try to decode a dict of dict (string) ==> should be an easier way
            let errorObj = Js.Json.decodeObject(Json.parseOrRaise(message));
            let errors: Js.Dict.t(Js.Dict.t(string)) = Js.Dict.empty();

            Belt.Option.getExn(errorObj)
            |> Js.Dict.entries |> Array.forEach(_,((key,v1)) => {
              let fieldObj = v1 |> Js.Json.decodeObject;
              switch(fieldObj){
                | Some(fieldObjErrs) => {
                  let fieldDict: Js.Dict.t(string) = Js.Dict.empty();
                  fieldObjErrs |> Js.Dict.entries |> Array.forEach(_, 
                    ((k,v)) => Js.Dict.set(fieldDict, k, v|> Js.Json.decodeString |> Belt.Option.getExn));
                  
                  Js.Dict.set(errors, key, fieldDict);
                }
                | None => ()
              }
            }
            );

            setState(_=> SaveFail(currentEntity, None, Some(errors)));
            
          }) {
            | x => {
              Js.log3("SaveFail: err msg decode failure for msg: ", x, message);
              setState(_=> SaveFail(currentEntity, Some({j|fail on decode: $message|j}), None));
            }
          }

          // // Decode the message? (or should be already decoded) 
          // let errors: Js.Dict.t(Js.Dict.t(string)) = Js.Dict.empty();
          // Js.log2("Err response: ", message);
          // setState(_=> SaveFail(currentEntity, None, Some(errors)));

        } 
      };
      Js.Promise.resolve();
    })
    |> ignore;

  };

  (switch(state) {
    | SaveFail(_,_  ,_)
    | Invalid(_,_) => {
      <div id="entity">
        <button style=(ReactDOMRe.Style.make(~display="none", ())) 
          onClick=(_=>()) >(str("Save"))</button>
        <button onClick=(_=>cancelAction()) >(str("Cancel"))</button>
        <h3 className="shadow" >(str("Edit entity: " ++ resource.title ))</h3>
        <div className="box-shadow border-red text-red">(str("Form Errors!"))</div>
        (printEntity())

      </div>
    }
    | Modified(currentEntity) => {
      <div id="entity">
        <button onClick=save(currentEntity) >(str("Save"))</button>
        <button onClick=(_=>cancelAction()) >(str("Cancel"))</button>
        <h3 className="shadow" >(str("Edit entity: " ++ resource.title ++ "/" ++ id ))</h3>
        (printEntity())
      </div>
    }    
    | Edit(_) => {
      <div id="entity">
        // Display the button; DOM refresh mouse focus otherwise
        <button style=(ReactDOMRe.Style.make(~display="none", ())) 
          onClick=(_=>()) >(str("Save"))</button>
        <button onClick=(_=>cancelAction()) >(str("Cancel"))</button>
        <h3 className="shadow" >(str("Edit entity: " ++ resource.title ++ "/" ++ id ))</h3>
        (printEntity())
      </div>
    }
    | Saving(currentEntity) =>{
      <div>
        <h3 className="shadow" >(str("Saving entity: " ++ resource.title ++ "/" ++ id ++ " \2026"  ))</h3>
        (currentEntity |> printAsJson)
      </div>
    }
    | SaveSuccess(newEntity) => {
      <div>
        <h3 className="shadow" >(str("Save success for: " ++ resource.title ++ "/" ++ id ))</h3>
        (newEntity |> printAsJson)
      </div>
    }
    | _ => ReasonReact.null
  })

};