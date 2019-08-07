open Common;
open Metadata;
open Store;
open EntityModules;

type t = array((Js.Json.t, array(Js.Json.t)));

type state = webLoadingData(option(t));

type channelState =
  | Unchanged(array(Js.Json.t))
  | Modified(array(Js.Json.t))
  | Invalid(array(Js.Json.t), Belt.Map.String.t(string));

type editState =
  | Edit
  | View;

let printFieldHeaderCell = (field: Field.t): ReasonReact.reactElement =>
  <th
    key={field.resource_name ++ "-" ++ field.name}
    className="cell cell-header">
    <div className="cycle-table-cell"> {str(field.title)} </div>
  </th>;

// NOTE: Not using EntityEditTableRowView: 
// Decided to devolve components: ChannelRow, ChannelTable, CycleRow
// For:
// - full control of render (sizing, styling) required
// - simpler than adding callbacks and "customEditor" functions.
module ChannelRow = {
  [@react.component]
  let make =
      (
        ~entity,
        ~resource: Resource.t,
        ~onChange: Js.Json.t => unit,
        ~onInvalid: (Js.Json.t, Js.Dict.t(Js.Dict.t(string))) => unit,
      ) => {
    let render = (entity: Js.Json.t, state: EntityEditor.state, setState) => {
      module EditFunctor =
        EntityEditor.Make({
          let resource = resource;
          let state = state;
          let setState = setState;
          let entity = entity;
        });

      React.useEffect1(
        () => {
          switch (state) {
          | Modified(_) =>
            Js.log2("calling chgFun", EditFunctor.getUpdatedEntity());
            onChange(EditFunctor.getUpdatedEntity() |> Js.Json.object_);
          | Invalid(_, currErrs) =>
            onInvalid(EditFunctor.getUpdatedEntity() |> Js.Json.object_, currErrs)
          | _ => ()
          };
          Some(() => Js.log("cleanup Effect"));
        },
        [|state|],
      );

      resource.fields
      |> Belt.Array.mapWithIndex(
           _,
           (i, field) => {
             let classForMod =
               "cycle-table-cell border border-2 "
               ++ (
                 EditFunctor.isFieldModified(field)
                   ? "modified-field" : "border-white"
               );
             <td key={string_of_int(i) ++ field.name}>
               //  onChange={_evt => notifyChange()}

                 <div className=classForMod>
                   {if (field.editable) {
                      EditFunctor.printEditor(field);
                    } else {
                      EditFunctor.printCurrentDisplayValue(field);
                    }}
                 </div>
               </td>;
           },
         )
      |> ReasonReact.array;
    };

    <EntityEditor entity render />;
  };
};

module ChannelTable = {
  [@react.component]
  let make =
      (
        ~channels,
        ~cycleChannelResource: Resource.t,
        ~editState=View,
        ~onChange: Js.Json.t => unit,
        ~onInvalid: (Js.Json.t, string) => unit,
      ) => {
    <table className="data-table" id="entity_table">
      <tbody>
        {switch (editState) {
         | Edit =>
           channels
           |> Belt.Array.mapWithIndex(_, (i, channel) =>
                <tr key={string_of_int(i)} className="hover:bg-grey-lighter">
                  <ChannelRow
                    resource=cycleChannelResource
                    entity=channel
                    onChange
                    // onChange={entity => onInvalid(entity, "test erorr")}
                    onInvalid={(entity, currErrs) => {
                      Js.log3("invalid channel row: ", entity, currErrs);

                      onInvalid(
                        entity,
                        EntityEditor.flattenErrors(currErrs)
                        |> Js.Array.joinWith("; "),
                      );
                    }}
                  />
                </tr>
              )
           |> ReasonReact.array
         | View =>
           channels
           |> Belt.Array.mapWithIndex(_, (i, channel) =>
                <tr key={string_of_int(i)} className="hover:bg-grey-lighter">
                  {cycleChannelResource.fields
                   |> Belt.Array.map(_, field =>
                        <td
                          key={"channel-" ++ field.name}
                          className="cell text-left">
                          <div className="cycle-table-cell">
                            {DetailView.EntityDetail.getFieldDisplayValue(
                               channel,
                               field,
                             )}
                          </div>
                        </td>
                      )
                   |> ReasonReact.array}
                </tr>
              )
           |> ReasonReact.array
         }}
      </tbody>
    </table>;
  };
};

module CycleTableRow = {
  [@react.component]
  let make =
      (
        ~cycleEntity,
        ~rowIndex,
        ~cycleResource: Resource.t,
        ~channels,
        ~cycleChannelResource: Resource.t,
        ~refreshAction,
      ) => {
    let (editState, setEditState) = React.useState(() => View);

    let (channelState, setChannelState) =
      React.useState(() => Unchanged(channels));

    let render = (entity: Js.Json.t, state: EntityEditor.state, setState) => {
      module EditFunctor =
        EntityEditor.Make({
          let resource = cycleResource;
          let state = state;
          let setState = setState;
          let entity = entity;
        });

      let printErrorMessage = optErrors =>
        <div id="form-errors" className="">
          <div className="text-red-500">
            {switch (optErrors) {
             | Some(errs) => arrayPrint(errs)
             | None => str("Form errors!")
             }}
          </div>
        </div>;

      let printEditEntity = () => {
        cycleResource.fields
        |> Belt.Array.mapWithIndex(
             _,
             (i, field) => {
               let classForMod =
                 "cycle-table-cell border border-2 "
                 ++ (
                   EditFunctor.isFieldModified(field)
                     ? "modified-field" : "border-white"
                 );
               if (field.name == "channels") {
                 <td
                   key={string_of_int(i) ++ "-cycle-" ++ field.name}
                   colSpan={Belt.Array.size(cycleChannelResource.fields)}>
                   <ChannelTable
                     channels
                     cycleChannelResource
                     editState=Edit
                     onChange={updatedChannel => {
                       let updateChannels =
                         CycleChannel.updateChannels(updatedChannel);
                       setChannelState(prevState =>
                         switch (prevState) {
                         | Unchanged(channels) =>
                           Modified(updateChannels(channels))
                         | Modified(channels) =>
                           Modified(updateChannels(channels))
                         | Invalid(channels, error) =>
                           Invalid(updateChannels(channels), error)
                         }
                       );
                     }}
                     onInvalid={(updatedChannel, error) => {
                       let updateChannels =
                         CycleChannel.updateChannels(updatedChannel);
                       setChannelState(prevState =>
                         switch (prevState) {
                         | Unchanged(channels)
                         | Modified(channels) =>
                           Invalid(
                             updateChannels(channels),
                             Belt.Map.String.fromArray([|
                               (CycleChannel.getId(updatedChannel), error),
                             |]),
                           )
                         | Invalid(channels, errors) =>
                           Invalid(
                             updateChannels(channels),
                             CycleChannel.updateErrors(
                               errors,
                               updatedChannel,
                               error,
                             ),
                           )
                         }
                       );
                     }}
                   />
                 </td>;
               } else {
                 <td key={string_of_int(i) ++ "-cycle-" ++ field.name}>
                   <div className=classForMod>
                     {if (field.editable) {
                        EditFunctor.printEditor(field);
                      } else {
                        EditFunctor.printCurrentDisplayValue(field);
                      }}
                   </div>
                 </td>;
               };
             },
           )
        |> ReasonReact.array;
      };

      let printViewEntity = () => {
        cycleResource.fields
        |> Belt.Array.mapWithIndex(
             _,
             (i, field) => {
               let classForMod =
                 "cycle-table-cell border border-2 "
                 ++ (
                   EditFunctor.isFieldModified(field)
                     ? "modified-field" : "border-white"
                 );
               switch (field.name) {
               | "channels" =>
                 <td
                   key={string_of_int(i) ++ field.name}
                   colSpan={Belt.Array.size(cycleChannelResource.fields)}>
                   <ChannelTable
                     key={field.name}
                     channels
                     cycleChannelResource
                     editState=View
                     onChange={_ => ()}
                     onInvalid={(_, _) => ()}
                   />
                 </td>
               | _ =>
                 <td key={string_of_int(i) ++ field.name}>
                   <div className=classForMod>
                     {DetailView.EntityDetail.getFieldDisplayValue(
                        cycleEntity,
                        field,
                      )}
                   </div>
                 </td>
               };
             },
           )
        |> ReasonReact.array;
      };

      let save = (): unit => {
        switch (state, channelState) {
        | (Modified(newCycleEntity), Modified(newChannels)) =>
          Js.log3("Save: ", newCycleEntity, newChannels)
        | (cycleState, channelState) =>
          Js.log3("Save fail, not Modified state: ", cycleState, channelState)
        };
      };

      let printCycleErrors =
          (state: EntityEditor.state): option(array(string)) => {
        switch (state) {
        | Invalid(_, errors) => Some(EntityEditor.flattenErrors(errors))
        | SaveFail(_, optError, optErrors) =>
          [|Belt.Option.getWithDefault(optError, "")|]
          |> Belt.Array.concat(
               _,
               Belt.Option.mapWithDefault(
                 optErrors,
                 [||],
                 EntityEditor.flattenErrors,
               ),
             )
          |> Belt.Array.keep(_, errmsg => Js.String.length(errmsg) > 0)
          |> (temp => Belt.Array.size(temp) > 0 ? Some(temp) : None)
        | _ => None
        };
      };
      let printChannelErrors = channelState =>
        switch (channelState) {
        | Invalid(_, errors) =>
          Some(
            Belt.Map.String.reduce(errors, [||], (cum, key, error) =>
              Js.Array.concat([|{j|channel: $key: $error |j}|], cum)
            ),
          )
        | _ => None
        };

      let getErrors = (state: EntityEditor.state, channelState) =>
        [|printCycleErrors(state), printChannelErrors(channelState)|]
        |> Belt.Array.keep(_, Belt.Option.isSome)
        |> Belt.Array.map(_, Belt.Option.getExn)
        |> Belt.Array.concatMany
        |> (errs => Belt.Array.size(errs) > 0 ? Some(errs) : None);

      let printButtons = () => {
        <td key="action-cell" className="cell text-left">
          <div className="cycle-table-cell">
            {switch (state, channelState) {
             | (Invalid(_), _)
             | (_, Invalid(_, _)) =>
               <>
                 <button
                   className="btn btn-gray"
                   style={ReactDOMRe.Style.make(~display="none", ())}
                   onClick={_ => save()}>
                   {str("Save")}
                 </button>
                 <button
                   className="btn btn-gray"
                   onClick={_ => {
                     setChannelState(_ => Unchanged(channels));
                     setState(_=>Edit(cycleEntity));
                     setEditState(_ => View);
                   }}>
                   //  refreshAction()
                    {str("Cancel")} </button>
                 {printErrorMessage(getErrors(state, channelState))}
               </>
             | (Modified(_), _)
             | (_, Modified(_)) =>
               <>
                 <button className="btn btn-gray" onClick={_ => save()}>
                   //  EditFunctor.save(
                   //    currentEntity,
                   //    ~isNew,
                   //    ~entityId,
                   //    ~saveAction,
                   //    (),
                   //  )
                    {str("Save")} </button>
                 <button
                   className="btn btn-gray"
                   onClick={_ => {
                     setChannelState(_ => Unchanged(channels));
                     setState(_=>Edit(cycleEntity));
                     setEditState(_ => View);
                   }}>
                   //refreshAction()
                    {str("Cancel")} </button>
               </>
             | (Edit(_), _) =>
               <>
                 <button
                   className="btn btn-gray"
                   style={ReactDOMRe.Style.make(~display="none", ())}
                   onClick={_ => ()}>
                   {str("Save")}
                 </button>
                 <button
                   className="btn btn-gray"
                   onClick={_ => {
                     setChannelState(_ => Unchanged(channels));
                     setEditState(_ => View);
                   }}>
                   {str("Cancel")}
                 </button>
               </>
             | (Saving(_currentEntity), _) => str("Saving...")
             | (SaveSuccess(_newEntity), _) =>
               <>
                 {str("Save success")}
                 <button
                   className="btn btn-gray"
                   style={ReactDOMRe.Style.make(~display="none", ())}
                   onClick={_ => ()}>
                   {str("Save")}
                 </button>
                 <button
                   className="btn btn-gray"
                   onClick={_ => {
                     setChannelState(_ => Unchanged(channels));
                     setEditState(_ => View);
                   }}>
                   {str("Cancel")}
                 </button>
               </>
             | (SaveFail(_, _, _), _) =>
               <>
                 <button
                   style={ReactDOMRe.Style.make(~display="none", ())}
                   className="btn btn-gray"
                   onClick={_ => ()}>
                   {str("Save")}
                 </button>
                 <button
                   className="btn btn-gray"
                   onClick={_ => {
                     setChannelState(_ => Unchanged(channels));
                     setEditState(_ => View);
                   }}>
                   {str("Cancel")}
                 </button>
                 {printErrorMessage(getErrors(state, channelState))}
               </>
             }}
          </div>
        </td>;
      };

      <tr key={"cycle_table_row-" ++ string_of_int(rowIndex)}>
        {switch (editState) {
         | View =>
           <>
             <td key="action-cell">
               <div className="cycle-table-cell">
                 <button
                   onClick={_evt => setEditState(_ => Edit)}
                   className="btn btn-gray">
                   {str("Edit")}
                 </button>
               </div>
             </td>
             {printViewEntity()}
           </>
         | Edit => <> {printButtons()} {printEditEntity()} </>
         }}
      </tr>;
    };
    <EntityEditor entity=cycleEntity render />;
  };
};

[@react.component]
let make =
    (
      ~experimentId,
      ~cycleResource: Resource.t,
      ~cycleChannelResource: Resource.t,
    ) => {
  let (state, setState) = React.useState(() => NotAsked);

  let fetchEntities = () => {
    let fetchChannels = cycles => {
      Js.log("fetchChannels...");
      ApiClient.getEntityListing(cycleChannelResource.name)
      |> Js.Promise.then_(result =>
           switch (result) {
           | Belt.Result.Ok(entities) =>
             Js.Promise.resolve(
               Belt.Result.Ok(
                 entities
                 |> Belt.Array.keep(_, entity =>
                      Belt.Array.some(cycles, cycle =>
                        CycleChannel.belongsTo(~entity, ~cycle)
                      )
                    ),
               ),
             )
           | result => Js.Promise.resolve(result)
           //  | Belt.Result.Error(message) =>
           //    setState(_ => LoadFailure(message))
           }
         );
    };

    Js.log("fetchEntities...");
    setState(_ => Loading);
    ApiClient.getEntityListing(cycleResource.name)
    |> Js.Promise.then_(result =>
         switch (result) {
         | Belt.Result.Ok(entities) =>
           entities
           |> Belt.Array.keep(_, entity =>
                Cycle.decode(entity).experiment_id == experimentId
              )
           |> (
             cycles => {
               fetchChannels(cycles)
               |> Js.Promise.then_(result => {
                    switch (result) {
                    | Belt.Result.Ok(channels) =>
                      Belt.Array.map(cycles, cycle =>
                        (
                          cycle,
                          Belt.Array.keep(channels, channel =>
                            CycleChannel.belongsTo(~entity=channel, ~cycle)
                          ),
                        )
                      )
                      |> (
                        cycle_table =>
                          Belt.Array.length(cycle_table) > 0
                            ? setState(_ => LoadSuccess(Some(cycle_table)))
                            : setState(_ => LoadSuccess(None))
                      )
                    | Belt.Result.Error(message) =>
                      setState(_ => LoadFailure(message))
                    };
                    Js.Promise.resolve();
                  });
             }
           )
         | Belt.Result.Error(message) =>
           setState(_ => LoadFailure(message));
           Js.Promise.resolve();
         }
       )
    |> ignore;
  };

  React.useEffect1(
    () => {
      if (state == NotAsked) {
        fetchEntities();
      };
      Some(() => Js.log("cleanup Effect"));
    },
    [|experimentId|],
  );

  let printTable = (cycleData: t) => {
    <table className="data-table" id="entity_table">
      <thead>
        <tr key="header" className="">
          {cycleResource.fields
           |> Belt.Array.keep(_, field => field.name != "channels")
           |> Belt.Array.map(_, field => printFieldHeaderCell(field))
           |> Belt.Array.concat([|
                <th key="action-column" className="cell cell-header">
                  {str("Action")}
                </th>,
              |])
           |> Belt.Array.concat(
                _,
                Belt.Array.map(
                  cycleChannelResource.fields,
                  printFieldHeaderCell,
                ),
              )
           |> ReasonReact.array}
        </tr>
      </thead>
      <tbody>
        {cycleData
         |> Belt.Array.mapWithIndex(_, (i, (cycleEntity, channels)) =>
              <CycleTableRow
                key={string_of_int(i)}
                cycleEntity
                rowIndex=i
                cycleResource
                channels
                cycleChannelResource
                refreshAction={_ => fetchEntities()}
              />
            )
         |> ReasonReact.array}
      </tbody>
    </table>;
  };

  switch (state) {
  | NotAsked => str("Not asked...")
  | LoadFailure(msg) => str("Load failure: " ++ msg)
  | Loading => str("Loading...")
  | LoadSuccess(cycleTableData) =>
    switch (cycleTableData) {
    | Some(cycleData) => <div> {printTable(cycleData)} </div>
    | None => str("No cycleTableData found")
    }
  };
};