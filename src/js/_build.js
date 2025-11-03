import { check_user_agent } from "./user_agent.js";
import { bind_button_actions } from "./buttons";
import { slider_context_labels } from "./ui.js";
import { search_osm, bind_search_events } from "./search.js";
import { bind_intro_actions, on_first_run } from "./intro.js";

(($) => {

  window.onload = () => {
    check_user_agent();
    window.search = search_osm
  }

  $(document).on("shiny:sessioninitialized", (event) => {
    on_first_run();
    bind_button_actions();
    bind_intro_actions();
    bind_search_events();
    slider_context_labels();
  })

})($)
