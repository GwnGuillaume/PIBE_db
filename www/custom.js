// JavaScript code for the Shiny app
$(document).on("shiny:connected", function(event) {
  Shiny.addCustomMessageHandler("downloadProgress", function(message) {
    var progressValue = message.progress * 100;
    $("#progress-bar").css("width", progressValue + "%");
    $("#progress-text").text("Downloading data... " + progressValue.toFixed(2) + "%");
  });
});