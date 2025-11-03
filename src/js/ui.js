const slider_context_labels = () => {
  const radiusSlider = document.querySelector('#filter_radius');
  if (!radiusSlider) return;

  const radiusContainer = radiusSlider.closest('.shiny-input-container');
  if (!radiusContainer) return;

  const labels = radiusContainer.getElementsByClassName("irs-grid-text");
  for (const label of labels) {
    try {
      const value = parseFloat(label.innerHTML);
      // Only format if it looks like a distance value (not a year)
      if (value > 0 && value <= 100) {
        (value < 1) && (label.innerHTML = `${parseInt(value * 1000)} m`);
        (value >= 1) && (label.innerHTML = `${parseInt(value)} km`);
      }
    } catch (error) {
      continue;
    }
  }
}

export {
  slider_context_labels
}
