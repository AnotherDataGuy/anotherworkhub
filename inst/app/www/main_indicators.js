document.querySelectorAll('.stat-value').forEach(valueElement => {
    let endValue = parseInt(valueElement.textContent);
    let increment = endValue / 100;

    for (let i = 0; i < 100; i++) {
        setTimeout(() => {
            valueElement.textContent = Math.min(endValue, parseInt(valueElement.textContent) + increment);
        }, 10 * i);
    }
});

$(document).on('shiny:connected', function(event) {
    setTimeout(function() {
        $('#recipient_of_the_pitch').attr('maxlength', '20');
        $('#recipients_background').attr('maxlength', '30');
        $('#recipient_activity').attr('maxlength', '20');
        $('#recipient_expertise').attr('maxlength', '20');
    }, 500); // Adjust delay as needed
});


function showTextSlowly() {
  setTimeout(function() {
    $('#section_pitch_improver_1-recap_prompt').css('opacity', '1');
  }, 100); // Small delay to ensure the text is rendered before starting the transition
}

