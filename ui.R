library(shiny)
library(r2d3)
library(shinyjs)
library(speechcollectr)

addResourcePath(
  prefix = "lineups",
  directoryPath = normalizePath(
    "Images",
    winslash = "/",
    mustWork = TRUE
  )
)

ui <- tagList(
  useShinyjs(),
  useRecorder(),

  # load CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # hide tabs before JS runs
  tags$head(tags$style(HTML('
    #topnav li a[data-value="Talk"],
    #topnav li a[data-value="Highlight"],
    #topnav li a[data-value="Summary"] {
      display: none;
    }
  '))),
  navbarPage(
    title = "Perception of Statistical Graphics",
    id = "topnav",
    selected = "Consent",


    # Consent
    tabPanel(
      "Informed Consent",
      value = "Consent",
      fluidPage(
        div(
          class = "consent-page",
          tags$div(
            class = "consent-main-title",
            "Perception and Decision Making Using Statistical Graphs"
          ),

          # Horizontal line
          tags$div(class = "consent-divider"),

          # Subtitle under the line
          tags$div(
            class = "consent-subtitle",
            "Participant Consent"
          ),
          div(
            class = "consent-section",
            HTML("
      <h4>IRB Project ID #: 20178 </h4>
      <h4>Invitation:</h4>
      <p>This study is intended to assess how people perceive statistical graphs and charts, and how charts are used to make decisions. You may participate in this study if:</p>
      <ul>
        <li>You are above the age of consent in your jurisdiction (18+ in most states, 19+ in Nebraska and Alabama, 21+ in Mississippi)</li>
        <li>You have normal or corrected-to-normal vision</li>
        <li>(if relevant) Your computer has a working microphone and you are willing to have your voice recorded.</li>
      </ul>

      <h4>Purpose of the study</h4>
      <p>Statistical charts and graphs are everywhere – in news articles, advertisements, and on TV. We are interested in whether people read information from charts accurately, and whether certain types of charts are more useful when making data-informed decisions. Unfortunately, we know relatively little about how people read and perceive charts. This study is designed to address this gap in research by systematically investigating the use of charts in different tasks and contexts. </p>

      <h4>Procedure of the study </h4>
      <p>Participation in this study should require less than ___ minutes of your time. You will be asked to look at statistical charts and then answer questions or complete tasks based on the visualization and contextual information.<p>
      <p>You may be asked to estimate, predict, or make decisions based on one or more graphs. You will be able to provide an explanation of your response, if you choose to do so. </p>
      <p>(If relevant) ) We may ask you to talk out loud as you think through the task, and record this information using your computer’s microphone. These recordings will be saved to the server and transcribed, but will be anonymous and should not contain any personally identifying information. <p>
      <p>We expect that each of the questions we ask will take less than 3 minutes to complete. We will start out with practice questions so that you can become accustomed to the interface. After the practice task(s), there will be a series of questions. At the end of the study, you will be asked for some demographic information, such as your age, education level, and occupation.<p>
      <p>Participation will take place in a location of your choosing, on your computer.</p>

      <h4>Possible risks of being in this research study</h4>
      <p>There are no known risks to you from participating in this study.</p>

      <h4>Possible benefits</h4>
      <p>You are not expected to receive any direct benefit from this study.</p>

      <h4>Compensation</h4>
      <p><strong>Prolific participants:</strong> We will pay you $XX for participating in this study through Prolific. At the conclusion of this study, you will be provided a URL that will direct you back to Prolific, signaling study completion. While you are free to quit at any time or to decline to answer any question, you will only be compensated if you complete the study and click on the redirect URL provided at the end of the study.</p>
      <p><strong>Reddit participants:</strong> You will not be paid for participation.</p>
      <p>Reddit statement (study run through Prolific as well as Reddit): You will not be paid to take part in this study if you participate through Reddit. In recognition of the fact that not all individuals who want to contribute to science are comfortable providing identifying information to an outside service, we have designed this study with two different participation options. If you wish to be compensated for your participation, you may register with Prolific and locate the study on that platform. <p>

      <h4>Confidentiality</h4>
      <p>Reasonable steps will be taken to protect the privacy and the confidentiality of your study data; however, in some circumstances we cannot guarantee absolute privacy and/or confidentiality. This study will never link personally identifying information with your responses.</p>
      <p>If you are participating through an outside service, such as Prolific, your participation will be recorded and linked to your Prolific account so that you can receive payment. At no point will any identifiable information be linked with your data: any responses you provide are completely anonymous. </p>
      <p>Research records will be securely stored electronically through University approved methods and will only be seen by the research team and/or those authorized to view, access, or use the records during the study.<p>
      <p>Any audio recordings we collect will be stored securely. Recordings will be transcribed using a computer and checked for accuracy by one of the researchers conducting the study. We do not expect any personally identifying information will be present on these recordings. Anonymization techniques will be applied to disguise your voice before recordings are shared. Any personally identifying information will be manually redacted from the transcripts and from audio recordings.<p>
      <p>The researchers conducting this study attempt to make all collected data available to the public to ensure scientific reproducibility; however, in the case of recorded audio, only anonymized audio files and transcripts will be shared with the public.<p>
      <p>Those who will have access to your research records are the study personnel, the Institutional Review Board (IRB), and any other person, agency, or sponsor as required by law or contract or institutional responsibility. The information from this study may be published in scientific journals or presented at scientific meetings. The individual data records, plus summaries and group-level analyses, will be published, but your identity will be kept strictly confidential.<p>

      <h4>Your rights as a participant</h4>
      <p>You may ask any questions about this research before or during your participation.</p>
      <ul>
        <li>For study-related questions, contact: <strong>Susan Vanderplas (susan.vanderplas@unl.edu)</strong></li>
        <li>For questions about your rights or complaints about the research, contact the Institutional Review Board (IRB):<br>
            Phone: (402) 472-6965   Email: irb@unl.edu
        </li>
      </ul>

      <h4>What will happen if you decide not to be in this research study or decide to stop participating once you start? </h4>
      <p>You can decide not to be in this research study, or you can stop being in this research study (“withdraw’) at any time before, during, or after the research begins for any reason. Deciding not to be in this research study or deciding to withdraw will not affect your relationship with the investigator or with the University of Nebraska-Lincoln.</p>
      <p>You will not lose any benefits to which you are entitled. However, if you withdraw before you receive a redirect link, we cannot compensate you for participation in the study.</p>
      <p>If you wish to withdraw from the study after completion, we will attempt to remove your data. To facilitate this process, you will need to provide the time you started and ended the study as well as any details you can remember about your responses. If we cannot uniquely identify a set of responses from the information you provide,  we may not be able to delete your responses from the study because we have designed the study to ensure that we are not collecting identifiable information. <p>

      <h4>Documentation of Informed Consent</h4>
      <p>You are voluntarily making a decision whether or not to participate in this research study. By clicking on the I Agree button below, your consent to participate is implied. </p>
      ")
          ),
          tags$div(class = "consent-divider"),
          div(
            class = "consent-actions",
            tags$strong("I have read the informed consent document and agree to participate in this experiment"),
            br(), br(),
            checkboxInput(
              "consent_choice",
              "I agree. You may save my data.",
              value = FALSE
            ),
            actionButton("consent_continue", "Continue", class = "btn btn-primary")
          )
        )
      )
    ),


    # Lineup landing page
    tabPanel(
      "Lineup Study",
      value = "Lineup",
      fluidPage(
        div(
          class = "hero",
          div(class = "hero-title", "Which plot is the most different?"),
          div(
            class = "hero-sub",
            "You’ll complete 12 short trials. Each trial shows a lineup of 20 plots. ",
            "Your goal is to identify the panel that stands out and complete a short task."
          )
        ),
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "task-card",
              tags$div(
                style = "font-size:19px; font-weight:800; margin-bottom:10px;",
                "How the study works"
              ),
              tags$ol(
                class = "step-list",
                tags$li(tags$b("Click Start the experiment"), " to begin Trial 1."),
                tags$li("Each trial shows a lineup of ", tags$b("20 panels"), " (numbered 1–20)."),
                tags$li("Choose the panel that looks ", tags$b("most different"), ".")
              )
            ),
            div(
              class = "task-card",
              tags$div(
                style = "font-size:19px; font-weight:800; margin-bottom:12px;",
                "What you will do in each trial"
              ),
              fluidRow(
                column(
                  4,
                  div(
                    class = "mini-card",
                    tags$div(class = "mini-title", HTML("🎤&nbsp; Talk aloud")),
                    tags$ul(
                      class = "mini-list",
                      tags$li(tags$b("Start recording"), " to reveal the lineup."),
                      tags$li("If prompted, allow microphone access"),
                      tags$li("Talk through what you notice."),
                      tags$li("Stop recording when you are done"),
                      tags$li(tags$b("Select panel number(s)"), "that looks different."),
                      tags$li("Then click ", tags$b("Save & proceed"), ".")
                    )
                  )
                ),
                column(
                  4,
                  div(
                    class = "mini-card",
                    tags$div(class = "mini-title", HTML("🖍&nbsp; Highlight")),
                    tags$ul(
                      class = "mini-list",
                      tags$li("Click a panel to select it."),
                      tags$li("Click ", tags$b("Highlight"), " and draw around features."),
                      tags$li("Mark what supports your choice (gaps, clusters, outliers, spread)."),
                      tags$li("Click ", tags$b("Save & proceed"), " when finished.")
                    ),
                    actionButton(
                      "show_highlight_demo",
                      "▶ View highlight demo",
                      class = "btn btn-warning btn-sm",
                      style = "width:100%; margin-top:8px; font-weight:600;"
                    )
                  )
                ),
                column(
                  4,
                  div(
                    class = "mini-card",
                    tags$div(class = "mini-title", HTML("✍&nbsp; Text summary")),
                    tags$ul(
                      class = "mini-list",
                      tags$li(tags$b("Select panel number(s)"), " (Selection 1 required)."),
                      tags$li("Write 1–2 sentences describing what makes it different."),
                      tags$li("Optional: add a second choice and explain it."),
                      tags$li("Click ", tags$b("Save & proceed"), " to continue.")
                    )
                  )
                )
              ),
              br(),
              tags$div(
                style = "font-size:16px; color:#b45309; margin-top:12px;",
                HTML("<b>Note:</b> These instructions will also appear on the left side of the page during each trial.")
              )
            ),
            div(
              class = "task-card",
              tags$div(
                style = "font-size:14px; color:#64748b;",
                HTML("⚠ <b>Please do not refresh the page</b> after you start — it may interrupt your session and recordings.")
              )
            ),
            br(),
            div(
              style = "text-align:center;",
              actionButton(
                "start_experiment",
                "Start the experiment",
                class = "btn btn-primary big-btn",
                style = "width:55%;"
              )
            ),
            br(), br()
          )
        )
      )
    ),


    # Talk aloud
    tabPanel(
      "Talk Aloud",
      value = "Talk",
      fluidPage(
        div(class = "task-title", "Talk aloud"),
        div(class = "task-sub", textOutput("trial_counter_talk")),
        fluidRow(
          column(
            4,
            div(
              class = "task-card",
              tags$div(style = "font-size:16px; font-weight:700; margin-bottom:8px;", "Instructions"),
              tags$ol(
                class = "step-list",
                tags$li(tags$b("Click Start recording"), " (the lineup will appear)."),
                tags$li("If prompted, allow microphone access"),
                tags$li(tags$b("Speak out loud:"), " Explain which panel seems different and why."),
                tags$li(tags$b("Stop recording"), " when you are done."),
                tags$li(tags$b("Select the panel number"), " that looks different."),
                tags$li(tags$b("Save & proceed"), " to move to the next trial.")
              ),
              div(
                class = "hint",
                tags$b("What to say (example): "),
                "“Most panels show a similar pattern, but panel # has more spread at the end and more extreme values. I think # is different.”"
              )
            ),
            div(
              class = "task-card",
              tags$div(style = "font-size:16px; font-weight:700; margin-bottom:10px;", "Recording controls"),
              actionButton("rec_start", "Start recording", class = "btn btn-primary btn-lg", width = "100%"),
              br(), br(),
              shinyjs::hidden(
                actionButton("rec_stop", "Stop recording", class = "btn btn-warning btn-lg", width = "100%")
              ),
              br(), br(),
              tags$div(style = "font-size:15px; color:#111827;", textOutput("rec_status")),
              tags$div(style = "font-size:13px; color:#6b7280;", textOutput("rec_timer")),
              tags$hr(style = "margin:12px 0;"),
              tags$div(style = "font-size:16px; font-weight:700; margin-bottom:10px;", "Your selection"),
              tags$div(style = "font-size:13px; font-weight:700; margin-bottom:6px;", "Selection 1 (required)"),
              selectInput("talk_sel1", NULL, choices = c("-", as.character(1:20)), selected = "-", width = "100%"),
              tags$div(
                style = "display:flex; justify-content:space-between; align-items:center; margin:10px 0 6px;",
                tags$span(style = "font-size:13px; font-weight:700;", "Selection 2 (optional)"),
                tags$span(style = "font-size:12px; color:#6b7280;", "Leave “-” if none")
              ),
              selectInput("talk_sel2", NULL, choices = c("-", as.character(1:20)), selected = "-", width = "100%"),
              br(),
              actionButton("talk_next", "Save & proceed to next trial", class = "btn btn-success btn-lg", width = "100%")
            )
          ),
          column(
            8,
            div(
              class = "lineup-shell",
              shinyjs::hidden(
                div(
                  id = "talk_placeholder", class = "placeholder",
                  "The lineup will appear here after you start recording."
                )
              ),
              shinyjs::hidden(
                div(
                  id = "talk_lineup_wrap",
                  d3Output("lineup_talk", height = "78vh")
                )
              )
            )
          )
        )
      )
    ),


    # Highlight tab
    tabPanel(
      "Highlight",
      value = "Highlight",
      fluidPage(
        div(class = "task-title", "Highlight"),
        div(class = "task-sub", textOutput("trial_counter_highlight")),
        fluidRow(
          column(
            4,
            div(
              class = "task-card",
              tags$div(style = "font-size:16px; font-weight:700; margin-bottom:8px;", "Instructions"),
              tags$ol(
                class = "step-list",
                tags$li(tags$b("Select"), " a panel that looks different."),
                tags$li(HTML('Click <b style="color:#b45309;">Highlight</b> and mark key features on the selected panel.')),
                div(
                  style = "margin:6px 0 12px 22px;",
                  actionButton(
                    "show_highlight_demo",
                    "▶ View highlight demo",
                    class = "btn btn-warning btn-sm"
                  )
                ),
                tags$li("If you selected a panel, make sure you highlight that same panel before continuing."),
                tags$li("If you have a second choice, select that panel and highlight."),
                tags$li(tags$b("Save & proceed"), " to move to the next trial.")
              ),
              div(
                class = "hint",
                tags$b("What to highlight: "),
                "clusters, gaps, unusual spread, outliers, slope/shape changes, or anything that supports your choice."
              )
            ),
            div(
              class = "task-card",
              tags$div(style = "font-size:16px; font-weight:700; margin-bottom:10px;", "When you're done"),
              actionButton("save_highlight", "Save & proceed to next trial", class = "btn btn-success btn-lg", width = "100%")
              # div(style = "font-size:15px; color:#6b7280; margin-top:10px;", "You can clear and redraw highlights if needed.")
            )
          ),
          column(8, d3Output("lineup_highlight", height = "80vh"))
        )
      )
    ),


    # Text Summary tab
    tabPanel(
      "Summary",
      value = "Summary",
      fluidPage(
        div(class = "task-title", "Text summary"),
        div(class = "task-sub", textOutput("trial_counter_summary")),
        fluidRow(
          column(
            4,
            div(
              class = "task-card",
              tags$div(style = "font-size:16px; font-weight:700; margin-bottom:8px;", "Instructions"),
              tags$ol(
                class = "step-list",
                tags$li(tags$b("Pick a plot"), " that looks different from the others."),
                tags$li(tags$b("Explain why"), " in 1–2 sentences (what visual feature stand out?)."),
                tags$li("Select a second plot if you have another choice and explain why."),
                tags$li(tags$b("Save & proceed"), " to move to the next trial,")
              ),
              div(
                class = "hint",
                tags$b("Good reasons mention: "),
                "shape/slope changes, spread differences, clusters, gaps, outliers, or unusual patterns."
              )
            ),
            div(
              class = "task-card",
              tags$div(style = "font-size:16px; font-weight:700; margin-bottom:10px;", "Your response"),
              tags$div(style = "font-size:13px; font-weight:700; margin-bottom:6px;", "Selection 1 (required)"),
              selectInput("sum_sel1", NULL, choices = c("-", as.character(1:20)), selected = "-", width = "100%"),
              textAreaInput(
                "sum_why1", NULL,
                placeholder = "Example: Has larger spread at high values and more extreme points near the end.",
                rows = 4, width = "100%"
              ),
              tags$hr(style = "margin:10px 0;"),
              tags$div(
                style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
                tags$span(style = "font-size:13px; font-weight:700;", "Selection 2 (optional)"),
                tags$span(style = "font-size:12px; color:#6b7280;", "Leave “-” if none")
              ),
              selectInput("sum_sel2", NULL, choices = c("-", as.character(1:20)), selected = "-", width = "100%"),
              textAreaInput("sum_why2", NULL, placeholder = "Optional second choice…", rows = 3, width = "100%"),
              br(),
              actionButton("submit_summary", "Save & proceed to next trial", class = "btn btn-success btn-lg", width = "100%")
            )
          ),
          column(8, d3Output("lineup_summary", height = "80vh"))
        )
      )
    ),


    # Demographics
    tabPanel(
      "Demographics",
      value = "Demographics",
      fluidPage(
        div(
          class = "demo-page",
          h3("Demographic Information"),
          selectInput(
            "demo_exp", "Age Range:",
            c(
              "", "Under 19", "19-25", "26-30", "31-35", "36-40", "41-45",
              "46-50", "51-55", "56-60", "Over 60", "Prefer not to say"
            ),
            selected = ""
          ),
          selectInput(
            "education", "Highest Educational Level:",
            c(
              "", "High school or Less", "Some Undergraduate Courses",
              "Undergraduate Degree", "Some Graduate Courses",
              "Graduate Degree", "Prefer not to say"
            ),
            selected = ""
          ),
          selectInput(
            "gender_identity", "Gender Identity:",
            c("", "Female", "Male", "Prefer not to say"),
            selected = ""
          ),
          selectInput(
            "color_blind",
            "Do you have a color deficiency (color blindness)?",
            c("", "No", "Yes", "Unsure", "Prefer not to say"),
            selected = ""
          ),
          br(), br(),
          actionButton("demo_continue", "Continue",
            class = "btn btn-primary"
          )
        )
      )
    ),


    # Done
    tabPanel(
      "Done",
      value = "Done",
      fluidPage(
        br(), br(),
        fluidRow(
          column(
            width = 8, offset = 2,
            div(
              class = "done-card",
              tags$h2("Thank You for Participating!"),
              br(),
              tags$p("Your responses have been successfully recorded.", style = "font-size:18px;"),
              tags$p("We truly appreciate the time and thought you put into completing this study.", style = "font-size:16px;")
            )
          )
        ),
        br(), br()
      )
    )
  )
)
