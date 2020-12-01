# Shadow Dom

Shadow DOM is a functionality that allows the web browser to render DOM elements without putting them into the main document DOM tree.

To access a Shadow DOM from [[javascript]], uses the `.shadowRoot` attributes of the parent element, for example in Gerrit UI:

```javascript
let commitContainer = document
  .getElementById("app")
  .shadowRoot.getElementById("app-element")
  .shadowRoot.querySelector("gr-change-view")
  .shadowRoot.querySelector(".commitContainer");
```
