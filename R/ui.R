library(shiny)
library(leaflet)
library(htmltools)

# Custom JavaScript and CSS styling adapted from the work of 
# Johnson Zhou's Parkit App.
# Original implementation reference:
#  https://github.com/johnsonjzhou/geom90007-a3
# Styles were modified and extended to fit the current 
# project's theme and UI structure.

headers <- tags$head(
  tags$link(
    rel = "icon",
    type = "image/x-icon",
    href = "assets/naarmwings-logo.svg"
  ),
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
  tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "assets/shiny_app.css"
  ),

  # mermaid.js for taxonomy diagrams
  # Core diagram rendering powered by Mermaid.js:
  # Knut Sveidqvist et al. (2014–2024)
  # Mermaid – Generation of diagrams and flowcharts from text in  
  # a similar manner as markdown.
  # https://mermaid.js.org/
  tags$script(
    src = "https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"
  ),
  # nolint start: line_length_linter
  tags$script(HTML(
    "
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
  "
  )),
  tags$script(HTML(
    "
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

          // Retry pattern to wait for DOM element and library loading
          // https://stackoverflow.com/questions/5525071/how-to-wait-until-an-element-exists
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
              // Mermaid rendering failed
            });
          }

          // requestAnimationFrame polyfill with 16ms fallback (60fps)
          // https://stackoverflow.com/questions/5605588/how-to-use-requestanimationframe
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
  "
  )),
  # nolint end
  tags$script(
    src = "assets/shiny_app.js"
  ),
  # helper to update Tableau viz year filter from Shiny
  # nolint start: line_length_linter
  tags$script(HTML(
    '
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
        }
      };
      tick();
    };

    window.updateTableauScientificName = function(id, scientificName) {
      let attempts = 25;
      const tick = async () => {
        try {
          const viz = document.getElementById(id);
          if (!viz || !viz.workbook) {
            if (attempts-- > 0) return void setTimeout(tick, 160);
            return;
          }
          const applyTo = async (sheet) => {
            if (!sheet) return;
            if (typeof sheet.applyFilterAsync === "function") {
              await sheet.applyFilterAsync("Scientific Name", [scientificName], "replace");
            }
            if (sheet.worksheets && sheet.worksheets.length) {
              for (const ws of sheet.worksheets) {
                try {
                  await ws.applyFilterAsync("Scientific Name", [scientificName], "replace");
                } catch (e) {
                  // Could not apply filter to worksheet
                }
              }
            }
          };
          await applyTo(viz.workbook.activeSheet);
        } catch (e) {
          if (attempts-- > 0) return void setTimeout(tick, 160);
        }
      };
      tick();
    };
  '
  ))
  # nolint end
)

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
      min = 1998,
      max = 2019,
      value = c(1998, 2019),
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

# Dimmer panel

dimmer_panel <- tabPanel(
  title = "Dimmer"
)

map_panel <- tabPanel(
  title = "Map",
  leafletOutput(
    "leaflet_map",
    height = "100%",
    width = "100%"
  )
)

search_panel <- tabPanel(
  title = "Search",
  fluidRow(
    class = "logo",
    tags$img(
      src = "assets/naarmwings-logo.svg"
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
        placeholder = "Search Location"
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
        "Explore bird observations across Melbourne (Naarm) from 1998 to 2019."
      )
    ),
    tags$div(
      class = "page",
      tags$img(src = "assets/slide2.svg"),
      tags$p(
        paste(
          "Find species near your location, filter by time period,",
          "rarity, and more."
        )
      )
    ),
    tags$div(
      class = "page",
      tags$img(src = "assets/slide3.svg"),
      tags$p(
        paste(
          "Discover the rich avian biodiversity of Melbourne",
          "with interactive maps and sounds."
        )
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

loading_panel <- tabPanel(
  title = "Loading",
  class = "container",
  tags$div(
    class = "pulse"
  )
)

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
