/**
  Handling of reactive event messages from Shiny
 */

/**
  Opens a tabPanel while adding effects
  @param {DOMElement} panel a DOM element representing a Shiny tabPanel
  @param {boolean} [dimmer=true] whether to toggle dimmer panel
  @return {boolean} True
 */
const activate_panel = (panel, apply_dimmer = true) => {
  if (!panel) {
    return false;
  }

  const dimmer = document.querySelector("[data-value='Dimmer'].tab-pane");
  panel.classList.add("active");

  if (apply_dimmer && dimmer) {
    dimmer.classList.add("dim");
    dimmer.addEventListener(
      "click",
      () => deactivate_panel(panel),
      { once: true }
    );
  }

  return true;
}

/**
  Closes a tabPanel while adding effects
  @param {DOMElement} panel a DOM element representing a Shiny tabPanel
  @param {boolean} [dimmer=true] whether to toggle dimmer panel
  @return {boolean} True
 */
const deactivate_panel = (panel, apply_dimmer = true) => {
  if (!panel) {
    return false;
  }

  const dimmer = document.querySelector("[data-value='Dimmer'].tab-pane");
  panel.classList.remove("active");

  if (apply_dimmer && dimmer) {
    dimmer.classList.remove("dim");
    dimmer.replaceWith(dimmer.cloneNode(true));
  }

  return true;
}

/**
  Loads and binds event handlers for Shiny custom messages
  @return void
 */
const load_event_handlers = () => {
  // Insert handlers here
}

export {
  activate_panel,
  deactivate_panel,
  load_event_handlers
};
