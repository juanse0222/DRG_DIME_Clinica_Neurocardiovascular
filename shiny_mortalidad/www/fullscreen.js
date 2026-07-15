/* GRD Dashboard — fullscreen panel toggle
 * Injects an expand button into every shinydashboard box header.
 * Works for static boxes AND boxes created via uiOutput (MutationObserver).
 */
$(document).ready(function () {

  // Single overlay element for dimming the background
  $('body').append('<div id="box-fs-overlay" class="box-fullscreen-overlay"></div>');

  // ── Inject expand button into every box that doesn't have one yet ──────────
  function addExpandBtns() {
    $('.box').each(function () {
      if ($(this).find('.box-expand-btn').length) return; // already added
      var $header = $(this).find('.box-header');
      if (!$header.length) return;
      // shinydashboard only adds .box-tools when collapsible=TRUE; create it if absent
      if (!$header.find('.box-tools').length) {
        $header.prepend('<div class="box-tools pull-right"></div>');
      }
      $header.find('.box-tools').prepend(
        '<button class="btn btn-box-tool box-expand-btn" title="Expandir panel">' +
        '<i class="fa fa-expand"></i></button>'
      );
    });
  }

  // Run once on load, then watch for dynamically rendered boxes (uiOutput)
  addExpandBtns();
  new MutationObserver(addExpandBtns).observe(document.body, {
    childList: true,
    subtree: true
  });

  // ── Exit fullscreen (shared logic) ─────────────────────────────────────────
  function exitFullscreen() {
    var $box = $('.box.box-fullscreen');
    if (!$box.length) return;
    $box.removeClass('box-fullscreen');
    $box.find('.box-expand-btn i').removeClass('fa-compress').addClass('fa-expand');
    $box.find('.box-expand-btn').attr('title', 'Expandir panel');
    $('#box-fs-overlay').hide();
    $('body').css('overflow', '');
    // Allow the DOM to relayout before asking Plotly to resize
    setTimeout(function () { $(window).trigger('resize'); }, 60);
  }

  // ── Toggle on button click ──────────────────────────────────────────────────
  $(document).on('click', '.box-expand-btn', function () {
    var $box  = $(this).closest('.box');
    var $icon = $(this).find('i');
    if ($box.hasClass('box-fullscreen')) {
      exitFullscreen();
    } else {
      $box.addClass('box-fullscreen');
      $icon.removeClass('fa-expand').addClass('fa-compress');
      $(this).attr('title', 'Restaurar panel');
      $('#box-fs-overlay').show();
      $('body').css('overflow', 'hidden');
      setTimeout(function () { $(window).trigger('resize'); }, 60);
    }
  });

  // Click the overlay (dimmed background) to exit
  $(document).on('click', '#box-fs-overlay', exitFullscreen);

  // ESC key to exit
  $(document).on('keydown', function (e) {
    if (e.key === 'Escape') exitFullscreen();
  });

});
