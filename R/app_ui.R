#' The application User-Interface
#'
#' @param request Internal parameter for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bs4Dash::dashboardPage(
      dark = TRUE,
      help = NULL,
      header = bs4Dash::dashboardHeader(
        title = tags$button(
          id = "btn_home_brand",
          class = "awh-brand-link awh-goto",
          type = "button",
          `data-target` = "home",
          "anotheRworkhub"
        ),
        titleWidth = "auto",
        rightUi = tags$li(
          class = "dropdown nav-item awh-header-lang-item",
          uiOutput("header_language_ui")
        ),
        uiOutput("user_profile")
      ),
      sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
      controlbar = NULL,
      footer = NULL,
      body = bs4Dash::dashboardBody(
        tags$div(
          class = "awh-main",
          uiOutput("auth_output"),
          div(id = "home_section", class = "awh-section", uiOutput("home_content")),
          div(id = "interview_section", class = "awh-section", style = "display: none;", uiOutput("interview_content")),
          div(id = "pitch_section", class = "awh-section", style = "display: none;", uiOutput("pitch_content"))
        ),
        uiOutput("bottom_navigation_ui")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    # CSS/JS under inst/app/www are injected once by bundle_resources() below.
    tags$script(HTML("
      (function() {
        function registerInterviewStreamHandler() {
          if (window.interviewStreamHandlerRegistered || !window.Shiny) return;
          window.interviewStreamHandlerRegistered = true;
          Shiny.addCustomMessageHandler('interview_stream_token', function(data) {
            var bubble = document.getElementById(data.id);
            if (!bubble) return;
            var textNode = bubble.querySelector('.streaming-text');
            if (textNode) {
              textNode.textContent = data.text || '';
            } else {
              bubble.textContent = data.text || '';
            }
            if (data.done) {
              bubble.classList.add('is-complete');
            } else {
              bubble.classList.remove('is-complete');
            }
            var container = bubble.closest('.chat-container');
            if (container) {
              container.scrollTop = container.scrollHeight;
            }
          });
        }
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', registerInterviewStreamHandler);
        } else {
          registerInterviewStreamHandler();
        }
        $(document).on('shiny:connected', registerInterviewStreamHandler);
      })();
    ")),
    tags$script("
      $(document).ready(function() {
        function awhReveal($el) {
          $el.css('display', 'block').removeClass('awh-section-enter');
          // Force reflow so the entrance animation restarts every switch.
          if ($el.length) { void $el[0].offsetWidth; }
          $el.addClass('awh-section-enter');
        }

        function awhSwitchSection(target) {
          var $show = $('#' + target + '_section');
          if (!$show.length || $show.is(':visible')) { return; }
          var $current = $('.awh-section:visible').not($show);
          window.scrollTo({ top: 0, behavior: 'smooth' });
          if ($current.length) {
            $current.stop(true, true).fadeOut(160, function() { awhReveal($show); });
          } else {
            awhReveal($show);
          }
        }

        function awhNav(target) {
          $('.bottom-sidebar-menu .nav-link, .bottom-sidebar-menu .btn').removeClass('active');
          $('#btn_' + target).addClass('active');
          awhSwitchSection(target);
        }

        // Home is the landing view; the other sections start hidden.
        $('#home_section').css('display', 'block');
        $('#interview_section').hide();
        $('#pitch_section').hide();

        $(document).on('click', '#btn_home', function() { awhNav('home'); });
        $(document).on('click', '#btn_home_brand', function() { awhNav('home'); });
        $(document).on('click', '#btn_interview', function() { awhNav('interview'); });
        $(document).on('click', '#btn_pitch', function() { awhNav('pitch'); });

        // In-page calls to action on the home screen reuse the nav logic.
        $(document).on('click', '.awh-goto', function() {
          var target = $(this).data('target');
          if (target) { awhNav(target); }
        });

        $(document).on('click', '#toggle-bottom-sidebar', function() {
          const sidebar = $('#bottom-sidebar');
          sidebar.toggleClass('minimized');
          $(this).find('i').toggleClass('fa-chevron-down fa-chevron-up');
        });

        // Fluid home: cursor glow + interactive step tabs + lane focus tint
        var $homeSection = $('#home_section');
        $homeSection.on('mousemove', function(e) {
          var rect = this.getBoundingClientRect();
          var x = ((e.clientX - rect.left) / rect.width) * 100;
          var y = ((e.clientY - rect.top) / rect.height) * 100;
          this.style.setProperty('--home-px', x + '%');
          this.style.setProperty('--home-py', y + '%');
          var dx = (x - 50) * 0.05;
          var dy = (y - 50) * 0.035;
          this.style.setProperty('--home-dx', dx + 'px');
          this.style.setProperty('--home-dy', dy + 'px');
        });

        $(document).on('mouseenter focus', '.home-lane', function() {
          $homeSection.attr('data-lane-focus', $(this).data('target') || '');
        });
        $(document).on('mouseleave blur', '.home-lane', function() {
          $homeSection.attr('data-lane-focus', '');
        });
      });
    "),
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "anotheRworkhub")
  )
}
