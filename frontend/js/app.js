$(function () {
  $("#run").on("click", function () {
    const action = $("#action").val();
    const params = $("#params").val();
    const payload = action + "|" + params;

    $("#output").text("Running...");

    $.ajax({
      url: "/api/run-cobol",
      type: "POST",
      contentType: "text/plain",
      data: payload,
    })
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
