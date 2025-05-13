$(function () {
  $("#run").on("click", function () {
    $("#output").text("Running...");
    $.get("/api/run-cobol.cgi")
      .done(function (data) {
        try {
          if (typeof data === "string") data = JSON.parse(data);
          $("#output").text(
            data.status === "ok"
              ? data.cobol
              : "Failed:\n" + JSON.stringify(data)
          );
        } catch (e) {
          $("#output").text("Unexpected response:\n" + data);
        }
      })
      .fail(function (jq, text, err) {
        $("#output").text("Error occurred: " + text + "\n" + err);
      });
  });
});
