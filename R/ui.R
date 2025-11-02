################################################################################
# UI components for the dashboard                                              #
################################################################################
library(shiny)
library(leaflet)
library(glue)
library(htmltools)

# Headers----------------------------------------------------------------------

headers <- tags$head(
  # favicon
  tags$link(
    rel = "icon",
    type = "image/x-icon",
    href = "assets/favicon.svg"
  ),
  # web fonts
  tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "https://use.typekit.net/zvh8ynu.css"
  ),
  tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "https://fonts.googleapis.com/icon?family=Material+Icons"
  ),
  # css overrides
  tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "assets/shiny_app.css"
  ),
  # mermaid.js for taxonomy diagrams
  tags$script(
    src = "https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"
  ),
  tags$script(HTML("
    mermaid.initialize({
      startOnLoad: false,
      theme: 'base',
      securityLevel: 'loose',
      themeVariables: {
        primaryColor: '#E8F5E9',
        primaryTextColor: '#2D5016',
        primaryBorderColor: '#4CAF50',
        lineColor: '#66BB6A',
        secondaryColor: '#C8E6C9',
        tertiaryColor: '#A5D6A7',
        fontSize: '14px',
        fontFamily: 'brandon-grotesque, Helvetica, Arial, sans-serif'
      },
      flowchart: {
        curve: 'basis',
        padding: 10
      }
    });
  ")),
  tags$script(HTML("
    (function() {
      var pendingDefinitions = {};

      function setupMermaidHandlers() {
        if (!window.Shiny) {
          return;
        }
        if (window.__mermaidHandlersReady) {
          return;
        }
        window.__mermaidHandlersReady = true;

        function renderMermaidDiagram(targetId, definitionOverride) {
          if (definitionOverride) {
            pendingDefinitions[targetId] = definitionOverride;
          }

          var attempts = 20;

          function tryRender() {
            var el = document.getElementById(targetId);
            if (!el) {
              if (attempts-- > 0) {
                window.setTimeout(tryRender, 60);
              }
              return;
            }

            var mermaid = window.mermaid;
            if (!mermaid) {
              if (attempts-- > 0) {
                window.setTimeout(tryRender, 60);
              }
              return;
            }

            if (el.dataset.processed === \"true\") {
              return;
            }

            var definition = definitionOverride ||
              pendingDefinitions[targetId] ||
              el.getAttribute(\"data-mermaid-definition\") ||
              el.dataset.mermaidDefinition ||
              (el.textContent || \"\");

            definition = (definition || \"\").trim();
            if (!definition) {
              return;
            }

            el.dataset.mermaidDefinition = definition;

            var svgId = targetId + \"-svg-\" + Math.floor(Math.random() * 1e6);
            var renderPromise;

            if (typeof mermaid.render === \"function\") {
              renderPromise = mermaid.render(svgId, definition);
            } else if (mermaid.mermaidAPI && typeof mermaid.mermaidAPI.render === \"function\") {
              renderPromise = new Promise(function(resolve, reject) {
                try {
                  mermaid.mermaidAPI.render(svgId, definition, function(svgCode) {
                    resolve({ svg: svgCode });
                  });
                } catch (err) {
                  reject(err);
                }
              });
            } else {
              if (attempts-- > 0) {
                window.setTimeout(tryRender, 60);
              }
              return;
            }

            Promise.resolve(renderPromise).then(function(result) {
              if (!result) {
                return;
              }
              el.innerHTML = result.svg || \"\";
              el.dataset.processed = \"true\";
              delete pendingDefinitions[targetId];
              if (typeof result.bindFunctions === \"function\") {
                result.bindFunctions(el);
              }
            }).catch(function(err) {
              console.error(\"Mermaid rendering failed for\", targetId, err);
            });
          }

          var raf = window.requestAnimationFrame || function(cb) { return window.setTimeout(cb, 16); };
          raf(tryRender);
        }

        Shiny.addCustomMessageHandler(\"renderMermaid\", function(message) {
          if (!message || !message.id) {
            return;
          }
          renderMermaidDiagram(message.id, message.definition);
        });

        if (window.jQuery) {
          window.jQuery(document).on(\"shown.bs.modal\", \".modal\", function() {
            try { window.dispatchEvent(new Event(\"resize\")); } catch (e) {}
            var blocks = this.querySelectorAll(\".mermaid:not([data-processed])\");
            if (!blocks.length) {
              return;
            }
            Array.prototype.forEach.call(blocks, function(block) {
              var id = block.getAttribute(\"id\");
              if (id) {
                renderMermaidDiagram(id);
              }
            });
          });
        }
      }

      if (window.Shiny) {
        setupMermaidHandlers();
      } else {
        document.addEventListener(\"shiny:connected\", setupMermaidHandlers, { once: true });
      }
    })();
  ")),
  # javascript
  tags$script(
    src = "assets/shiny_app.js"
  )
  ,
  # helper to update Tableau viz year filter from Shiny
  tags$script(HTML('
    window.updateTableauYearRange = function(id, minYear, maxYear) {
      let attempts = 25;
      const minDate = new Date(minYear, 0, 1);
      const maxDate = new Date(maxYear, 11, 31);
      const tick = async () => {
        try {
          const viz = document.getElementById(id);
          if (!viz || !viz.workbook) {
            if (attempts-- > 0) return void setTimeout(tick, 160);
            return;
          }
          const applyTo = async (sheet) => {
            if (!sheet) return;
            if (typeof sheet.applyRangeFilterAsync === "function") {
              await sheet.applyRangeFilterAsync("Date", { min: minDate, max: maxDate });
            }
            if (sheet.worksheets && sheet.worksheets.length) {
              for (const ws of sheet.worksheets) {
                try { await ws.applyRangeFilterAsync("Date", { min: minDate, max: maxDate }); } catch (e) {}
              }
            }
          };
          await applyTo(viz.workbook.activeSheet);
        } catch (e) {
          if (attempts-- > 0) return void setTimeout(tick, 160);
          console.error("updateTableauYearRange error", e);
        }
      };
      tick();
    };

    window.updateTableauScientificName = function(id, scientificName) {
      let attempts = 25;
      console.log("[updateTableauScientificName] Called for ID:", id, "ScientificName:", scientificName);
      const tick = async () => {
        try {
          const viz = document.getElementById(id);
          if (!viz || !viz.workbook) {
            console.warn("[updateTableauScientificName] Viz not found or workbook not loaded for ID:", id);
            if (attempts-- > 0) return void setTimeout(tick, 160);
            return;
          }
          const applyTo = async (sheet) => {
            if (!sheet) return;
            if (typeof sheet.applyFilterAsync === "function") {
              console.log("[updateTableauScientificName] Applying filter to activeSheet", id, scientificName);
              await sheet.applyFilterAsync("Scientific Name", [scientificName], "replace");
            }
            if (sheet.worksheets && sheet.worksheets.length) {
              for (const ws of sheet.worksheets) {
                try {
                  console.log("[updateTableauScientificName] Applying filter to worksheet", ws.name || ws, scientificName);
                  await ws.applyFilterAsync("Scientific Name", [scientificName], "replace");
                } catch (e) {
                  console.warn("Could not apply filter to worksheet", ws, e);
                }
              }
            }
          };
          await applyTo(viz.workbook.activeSheet);
        } catch (e) {
          console.error("[updateTableauScientificName] Error during filtering:", e);
          if (attempts-- > 0) return void setTimeout(tick, 160);
        }
      };
      tick();
    };
  '))
)

# Filter Panel-----------------------------------------------------------------

filter_panel <- tabPanel(
  title = "Filters",
  fluidRow(
    class = "header",
    tags$h1(
      "Filters"
    ),
    tags$div(
      id = "filters-show-hide",
      class = "button-show",
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    selectInput(
      inputId = "filter_species",
      label = "Species",
      choices = character(0),
      selected = NULL,
      multiple = TRUE
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    selectInput(
      inputId = "filter_order",
      label = "Order (Taxonomy)",
      choices = character(0),
      selected = NULL,
      multiple = TRUE
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_common",
      label = "Show Common birds",
      value = TRUE
    )
  ),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_fairly_common",
      label = "Show Fairly Common birds",
      value = TRUE
    )
  ),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_uncommon",
      label = "Show Uncommon birds",
      value = TRUE
    )
  ),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_rare",
      label = "Show Rare birds",
      value = TRUE
    )
  ),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_vagrant",
      label = "Show Vagrant birds",
      value = TRUE
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    tags$div(
      class = "label",
      "Observation Period"
    ),
    sliderInput(
      inputId = "filter_year_range",
      min = 1985,
      max = 2019,
      value = c(1985, 2019),
      step = 1,
      sep = "",
      label = NULL
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    tags$div(
      class = "label",
      "Search Radius"
    ),
    sliderInput(
      inputId = "filter_radius",
      min = 0,
      max = 8,
      step = 2,
      value = 8,
      label = NULL,
      post = "km"
    )
  ),
  tags$div(class = "spacer h32")
)

# Dimmer Panel-----------------------------------------------------------------

dimmer_panel <- tabPanel(
  title = "Dimmer"
)

# Map Panel---------------------------------------------------------------------

map_panel <- tabPanel(
  title = "Map",
  leafletOutput(
    "leaflet_map",
    height = "100%",
    width = "100%"
  )
)

# Search Panel-----------------------------------------------------------------

search_panel <- tabPanel(
  title = "Search",
  fluidRow(
    class = "logo",
    tags$img(
      src = "assets/logo.svg"
    )
  ),
  fluidRow(
    class = "search-bar",
    tags$div(
      class = "wrapper",
      textInput(
        inputId = "search-input",
        label = NULL,
        value = "Melbourne",
        placeholder = "Search Destination"
      ),
      tags$div(
        id = "button-gps",
        class = "button gps"
      ),
      tags$div(
        id = "button-search",
        class = "button search"
      )
    )
  )
)

search_results_panel <- tabPanel(
  title = "SearchResults"
)

# Intro panel------------------------------------------------------------------

intro_panel <- tabPanel(
  title = "Intro",
  class = "page-1",
  tags$img(class = "logo", src = "assets/naarmwings-logo.svg"),
  tags$div(
    class = "pages",
    tags$div(
      class = "page",
      tags$img(src = "assets/slide1.svg"),
      tags$p(
        "Explore bird observations across Melbourne (Naarm) from 1985 to 2019."
      )
    ),
    tags$div(
      class = "page",
      tags$img(src = "assets/slide2.svg"),
      tags$p(
        "Find species near your location, filter by time period, rarity, and more."
      )
    ),
    tags$div(
      class = "page",
      tags$img(src = "assets/slide3.svg"),
      tags$p(
        "Discover the rich avian biodiversity of Melbourne with interactive maps and sounds."
      )
    )
  ),
  tags$div(class = "bubble one"),
  tags$div(class = "bubble two"),
  tags$div(
    class = "dots",
    tags$div(class = "dot seq-1"),
    tags$div(class = "dot seq-2"),
    tags$div(class = "dot seq-3")
  ),
  tags$div(
    id = "intro-left",
    class = "button left",
    direction = "left"
  ),
  tags$div(
    id = "intro-right",
    class = "button right",
    direction = "right"
  )
)

# Loading panel----------------------------------------------------------------

loading_panel <- tabPanel(
  title = "Loading",
  class = "container",
  tags$div(
    class = "pulse"
  )
)

# UI element-------------------------------------------------------------------

ui <- tagList(
  headers,
  setUpTableauInShiny(),
  navbarPage(
    title = "NaarmWings",
    map_panel,
    search_panel,
    search_results_panel,
    dimmer_panel,
    loading_panel,
    filter_panel,
    intro_panel,
    header = NULL,
    windowTitle = "NaarmWings",
    fluid = FALSE,
    position = "fixed-top",
    lang = "en"
  )
)
