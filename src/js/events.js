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

const deactivate_panel = (panel, apply_dimmer = true) => {
  if (!panel) {
    return false;
  }

  const dimmer = document.querySelector("[data-value='Dimmer'].tab-pane");
  panel.classList.remove("active");

  if (apply_dimmer && dimmer) {
    dimmer.classList.remove("dim");
    // Remove all event listeners by replacing element with clone
    // https://stackoverflow.com/questions/9251837/how-to-remove-all-listeners-in-an-element
    dimmer.replaceWith(dimmer.cloneNode(true));
  }

  return true;
}

export {
  activate_panel,
  deactivate_panel
};
