import { Elm } from './Main.elm';

// Selecting tokens
const getDocSelection = () => {
  const selection = document.getSelection()
  // Focus is where the selection ended
  const focus = selection.focusNode.parentElement
  // Anchor is where the selection began
  const anchor = selection.anchorNode.parentElement
  // Reports relative positioning of nodes
  const position = selection.focusNode.compareDocumentPosition(
    selection.anchorNode)

  // Ensure that it is a token that is selected
  if (focus.classList.contains('token-cell')
      && anchor.classList.contains('token-cell')) {

    // The anchor precedes focus
    if (position == 2) {
      return JSON.stringify({ left: Math.floor(anchor.id),
                              right: Math.floor(focus.id),
                              first: Math.floor(anchor.id),
                              last: Math.floor(focus.id)
                            })
      // The focus precedes anchor
    } else if (position == 4) {
      return JSON.stringify({ right: Math.floor(anchor.id),
                              left: Math.floor(focus.id),
                              first: Math.floor(focus.id),
                              last: Math.floor(anchor.id)
                            })
      // Only a single token is selected
    } else if (selection.focusNode == selection.anchorNode) {
      return JSON.stringify({ left: Math.floor(anchor.id),
                              right: Math.floor(focus.id),
                              first: Math.floor(focus.id),
                              last: Math.floor(focus.id)
                            })
      // Otherwise, null
    } else {
      return JSON.stringify(null)
    }
  } else {
      return JSON.stringify(null)
  }
}

// Add a node for elm to attach
const $root = document.createElement('div')
document.body.appendChild($root)

// An element that holds data from the file openned as well as
// potential project data.
const dataelement = document.getElementById('dataelement')

// Initialize elm with flags.
const app = Elm.Main.init({
  node: $root,
  flags: {
    proj: dataelement.getAttribute('data-proj_text'),
    text: dataelement.getAttribute('data-ann_text')
  }
});

// Ensure the ports have been defined in elm. Then define them here.
if (app.ports) {
  if (app.ports.requestDocSelection && app.ports.receivedDocSelection) {
    app.ports.requestDocSelection.subscribe(() => {
      app.ports.receivedDocSelection.send(getDocSelection())
    })
  }
}
