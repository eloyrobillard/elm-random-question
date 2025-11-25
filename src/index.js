import "./styles.css";
import { Elm } from "./Main.elm";

// Mount "Main" Browser.{element,document} on #root
Elm.Main.init({
  node: document.querySelector("main"),
  flags: "Initial Message",
});
