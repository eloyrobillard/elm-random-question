import { Elm } from './Main.elm'

// Mount "Main" Browser.{element,document} on #root
Elm.Main.init({
  node: document.getElementById('myapp'),
  flags: "Initial Message"
})