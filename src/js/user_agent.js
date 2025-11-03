const check_user_agent = () => {
  navigator.userAgent.includes("QtWebEngine") &&
    window.alert("For best effect, please use an external browser.");
};

export {
  check_user_agent
};