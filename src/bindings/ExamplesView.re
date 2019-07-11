open Common;

[@react.component]
let make = () =>
  <div>
    {str("Auto Suggest: ")}
    <a target="_blank" href="https://github.com/moroshko/react-autosuggest">
      {str("github.com/moroshko/react-autosuggest")}
    </a>
    <AutosuggestExample />
    <hr/>
    {str("React-select: ")}
    <a target="_blank" href="https://github.com/JedWatson/react-select">
      {str("github.com/JedWatson/react-select")}
    </a>
    <ReactSelectExample />
    <hr/>
    {str("React-select multi: ")}
    <a target="_blank" href="https://github.com/JedWatson/react-select">
      {str("github.com/JedWatson/react-select")}
    </a>
    <ReactSelectMultiExample />
    <hr/>
    {str("RC-select: ")}
    <a target="_blank" href="https://github.com/react-component/select">
      {str("https://github.com/react-component/select")}
    </a>
    <RcSelectExample />    
    <hr/>
    {str("React dropdown-select: ")}
    <a target="_blank" href="https://github.com/sanusart/react-dropdown-select">
      {str("github.com/sanusart/react-dropdown-select")}
    </a>
    <ReactDropdownSelectExample />    
    <hr/>
    {str("File input example: ")}
    <FileInputExample />    
  </div>;