open Common;

// [%bs.raw {|require('css/modal.css')|}];

[@react.component]
let make = (~dispatchModal, ~show, ~customClass, ~children) => {

  <div className=("modal " ++ customClass) style=(
          ReactDOMRe.Style.make(~display=show ? "block" : "none", ())) >
    <div className="modal_content">
      <div >
      children
      </div>
      <button key="cancel" title="Close" className="close_modal"  onClick={_evt=>dispatchModal(ModalCancel)} >
        (str("Cancel"))
      </button>
      <button key="ok" title="Ok" className="close_modal" onClick={_evt=>dispatchModal(ModalOk) } >
        (str("Ok"))
      </button>
    </div>
  </div>
  
}