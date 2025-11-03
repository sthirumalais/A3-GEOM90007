import { activate_panel, deactivate_panel } from "./events.js"
import { search_panel_go, close_search_results, use_geolocation } from "./search.js";

const filters_show_hide = () => {
  const panel = document.querySelector("[data-value='Filters'].tab-pane");
  if (!panel) {
    return;
  }

  panel.classList.contains("active")
    ? deactivate_panel(panel)
    : activate_panel(panel);
}

const bind_button_actions = () => {
  const filters_toggle = document.getElementById("filters-show-hide");
  filters_toggle && filters_toggle.addEventListener(
    "click",
    filters_show_hide
  );

  const search_button = document.getElementById("button-search");
  search_button && search_button.addEventListener(
    "click",
    search_panel_go
  );

  const gps_button = document.getElementById("button-gps");
  gps_button && gps_button.addEventListener(
    "click",
    use_geolocation
  );

  const search_input = document.getElementById("search-input");
  search_input && search_input.addEventListener(
    "keydown",
    (event) => {
      switch (event.key) {
        case "Enter":
          event.preventDefault();
          event.stopPropagation();
          search_panel_go();
          break;
        case "Escape":
          event.preventDefault();
          event.stopPropagation();
          event.target.blur();
          close_search_results();
          break;
        default:
      }
    }
  );

  document.addEventListener("click", (event) => {
    // Close the SearchResults panel when clicking outside
    const res_panel = document.querySelector("[data-value='SearchResults'].tab-pane");
    const search_wrapper = document.querySelector(".search-bar .wrapper");

    // Only close if clicking outside both panel and search box
    if (res_panel && search_wrapper &&
        !res_panel.contains(event.target) &&
        !search_wrapper.contains(event.target)) {
      close_search_results();
    }
  });
}

export {
  bind_button_actions
};
