const SEARCH_INPUT_ID = "search-input";
const get_search_input = () => document.getElementById(SEARCH_INPUT_ID);

const search_osm = async (query) => {
  const trimmed_query = (query || "").trim();
  if (!trimmed_query) {
    return [];
  }

  // The URL endpoint
  const url = new URL("https://nominatim.openstreetmap.org/search");
  
  // Append params to the url
  const params = {
    "format": "json",
    "countrycodes": "au",
    "viewbox": "144.93366,-37.79264,144.97670,-37.82391",
    "bounded": "1",
    "q": trimmed_query
  }
  
  for (let param in params) {
    url.searchParams.append(param, params[param])
  }

  on_search_go();

  try {
    const result = await fetch(url);
    if (result.status !== 200) {
      throw new Error(`Bad Server Response (${result.status})`);
    }
    return await result.json();
  } catch (error) {
    return [];
  } finally {
    on_search_done();
  }
}

const render_search_results = (json) => {
  const res_panel = document.querySelector("[data-value='SearchResults'].tab-pane");
  if (!res_panel) {
    return;
  }

  res_panel.innerHTML = "";
  const results = Array.isArray(json) ? json : [];
  const res_elements = results.map((result) => {
    // Wrapper
    const wrapper = document.createElement("div");
    wrapper.classList.add("result-wrapper");
    
    // Wrangle the display_name
    const display_names = osm_display_name(result);
    
    // Bind click event to wrapper
    wrapper.addEventListener("click", () => {
      const input = get_search_input();
      if (!input) {
        return;
      }

      const { lat, lon } = result || {};
      if (lat == null || lon == null) {
        search_res_alert("Selected result is missing location data.", true);
        return;
      }
      
      input.dispatchEvent(
        new CustomEvent("set:loc", {"detail": result})
      );
      
      const res_panel_click = document.querySelector("[data-value='SearchResults'].tab-pane");
      res_panel_click && res_panel_click.dispatchEvent(
        new CustomEvent("close:panel", {"detail": result})
      );
      
      // Sets the search input box to display the selected name
      input.value = display_names[0] || "Unknown name";
      
      // Close the panel
      close_search_results();
      
      // Clean up any other indicators
      remove_gps_indicator();
    });
    
    // Create internal elements
    const name = document.createElement("div");
    name.classList.add("result-name");
    name.innerHTML = display_names[0] || "Unknown name";
    
    const loc = document.createElement("div");
    loc.classList.add("result-loc");
    loc.innerHTML = display_names[1] && display_names.slice(1).join(" ")
      || "Unknown location";
    
    wrapper.appendChild(name);
    wrapper.appendChild(loc);
    
    return wrapper;
  })
  
  // Check that there are elements in res_elements, if not, add a message
  if (res_elements.length < 1) {
    search_res_alert("No results found, please try again.");
  }
  
  // Add elements to the res_panel
  res_panel.append(... res_elements);
  
  // Open the panel after results have been rendered
  open_search_results();
}

const search_res_alert = (message, open_panel = false) => {
  const res_panel = document.querySelector("[data-value='SearchResults'].tab-pane");
  if (!res_panel) {
    return;
  }

  // Reset panel contents and add the message
  res_panel.innerHTML = "";
  const msg = document.createElement("div");
  msg.classList.add("result-none");
  msg.innerHTML = message;
  res_panel.append(msg);
  
  // Open the panel if required
  open_panel && open_search_results();
}

const osm_display_name = (obj) => {
  return obj.display_name && obj.display_name.split(",") || [];
}

const open_search_results = () => {
  const res_panel = document.querySelector("[data-value='SearchResults'].tab-pane");
  const search_input = get_search_input();
  if (!res_panel || !search_input) {
    return;
  }

  const input_wrapper = search_input.closest(".wrapper");
  if (!input_wrapper) {
    return;
  }

  const {
    bottom:y, width
  } = input_wrapper.getBoundingClientRect();
  
  res_panel.style.top = `${y}px`;
  res_panel.style.width = `${width}px`;
  res_panel.classList.add("active");
}

const close_search_results = () => {
  const res_panel = document.querySelector("[data-value='SearchResults'].tab-pane");
  res_panel && res_panel.classList.remove("active");
}

const search_panel_go = async () => {
  // Get the search query from the search input box
  const input = get_search_input();
  if (!input) {
    return;
  }

  const query = input.value || "";
  const trimmed_query = query.trim();
  if (!trimmed_query) {
    search_res_alert("Please enter a location.", true);
    return;
  }
  
  // Query the API
  const results = await search_osm(trimmed_query);
  
  // Render the search results
  render_search_results(results);
}

const on_search_go = () => {
  // Apply styling changes
  const panel = document.querySelector("[data-value='Search'].tab-pane");
  panel && panel.classList.add("busy");
  
  // Dispatch event
  const input = get_search_input();
  input && input.dispatchEvent(
    new CustomEvent("search:busy")
  );
}

const on_search_done = () => {
  // Apply styling changes
  const panel = document.querySelector("[data-value='Search'].tab-pane");
  panel && panel.classList.remove("busy");
  
  // Dispatch event
  const input = get_search_input();
  input && input.dispatchEvent(
    new CustomEvent("search:done")
  );
}

const use_geolocation = () => {
  const on_success = (location) => {
    //location.coords.[latitude, longitude]
    const {coords: {
      latitude:lat, longitude:lon
    }} = location;
    const input = document.getElementById("search-input");
      
    input.dispatchEvent(
      new CustomEvent("set:loc", {"detail": {lat, lon}})
    );
    
    search_res_alert("Current GPS location set.", true);
    set_gps_indicator();
  }
  
  const on_failure = (reason) => {
    //reason.message
    let message = "Unable to access current location.";
    if (reason) {
      switch (reason.code) {
        case reason.PERMISSION_DENIED:
          message = "Location permission was denied.";
        break;
        case reason.POSITION_UNAVAILABLE:
          message = "Location information is unavailable.";
        break;
        case reason.TIMEOUT:
          message = "Location request timed out. Please try again.";
        break;
        default:
          message = reason.message || message;
      }
    }
    search_res_alert(message, true);
  }
  
  // Query the navigator geolocation service
  navigator.geolocation &&
  navigator.geolocation.getCurrentPosition(on_success, on_failure);
}

const set_gps_indicator = () => {
  const icon = document.getElementById("button-gps");
  icon.classList.add("active");
}

const remove_gps_indicator = () => {
  const icon = document.getElementById("button-gps");
  icon.classList.remove("active");
}

const bind_search_events = () => {
  const res_panel = document.querySelector("[data-value='SearchResults'].tab-pane");
  const input = get_search_input();
  if (!input) {
    return;
  }

  // On results selection
  input.addEventListener("set:loc", (event) => {
    const { detail: data } = event || {};
    if (!data || data.lat == null || data.lon == null) {
      search_res_alert("Selected result is missing location data.", true);
      return;
    }
    if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
      Shiny.setInputValue("js_set_loc", data);
    }
  })
}

export {
  search_osm,
  search_panel_go,
  bind_search_events,
  open_search_results,
  close_search_results,
  use_geolocation
}
