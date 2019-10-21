'use strict';
import ClipboardJS from "clipboard";
import { Elm } from "./Main.elm";

window.Elm = Elm;

const clipboardScript = document.createElement("script");
window.ClipboardJS = ClipboardJS;
const inlineScript = document.createTextNode("new ClipboardJS('#copy-button');");
clipboardScript.appendChild(inlineScript);
document.body.appendChild(clipboardScript)

