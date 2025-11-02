/**
  Scripts handling the behaviour for Intro page
 */

const introPane = () => document.querySelector("[data-value='Intro'].tab-pane");

const updateIntroPage = (pane, desiredPage) => {
  if (!pane) return;

  const total =
    parseInt(pane.dataset.pageCount || "0", 10) ||
    pane.getElementsByClassName("page").length ||
    0;

  if (total < 1) return;

  const nextPage = Math.min(Math.max(desiredPage, 1), total);

  for (let i = 1; i <= total; i += 1) {
    pane.classList.remove(`page-${i}`);
  }

  pane.classList.add(`page-${nextPage}`);
  pane.dataset.currentPage = String(nextPage);
  pane.dataset.pageCount = String(total);
};

const on_intro_btn_click = (event) => {
  const target = event.currentTarget || event.target;
  const directionAttr = target && target.getAttribute("direction");
  const pane = introPane();

  if (!directionAttr || !pane) return;

  const current = parseInt(pane.dataset.currentPage || "1", 10) || 1;
  const total =
    parseInt(pane.dataset.pageCount || "0", 10) ||
    pane.getElementsByClassName("page").length ||
    0;

  if (directionAttr === "left") {
    updateIntroPage(pane, current - 1);
  } else if (directionAttr === "right") {
    if (current < total) {
      updateIntroPage(pane, current + 1);
    } else {
      pane.classList.remove("active");
    }
  }
};

/**
  Binds click events to buttons
  @returns {void}
 */
const bind_intro_actions = () => {
  const pane = introPane();
  if (!pane) return;

  const pageCount = pane.getElementsByClassName("page").length || 0;
  pane.dataset.pageCount = String(pageCount);
  pane.dataset.currentPage = "1";
  updateIntroPage(pane, 1);

  const btn_left = document.getElementById("intro-left");
  const btn_right = document.getElementById("intro-right");
  
  btn_left && btn_left.addEventListener("click", on_intro_btn_click);
  btn_right && btn_right.addEventListener("click", on_intro_btn_click);
};

/**
  Show the intro panel on first run
  @return {void}
 */
const on_first_run = () => {
  const pane = introPane();
  if (!pane) return;
  pane.classList.add("active");
  updateIntroPage(pane, 1);
};

export {
  bind_intro_actions,
  on_first_run
}
