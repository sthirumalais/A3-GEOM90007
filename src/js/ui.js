/**
  UI related scripts - Adapted for Bird Visualization
 */


/**
  Updates labels on the Radius slider to be more context friendly
  Only formats labels for the radius slider (distance in km/m)
  Leaves year range slider labels as plain years
  @return {void}
 */
const slider_context_labels = () => {
  // Find the radius slider specifically by its parent container
  const radiusSlider = document.querySelector('#filter_radius');
  if (!radiusSlider) return;

  // Only format labels within the radius slider
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
