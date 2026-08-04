#' Translation and label mappings for the application
#'
#' @return A list of bilingual strings and choice maps.
#' @noRd
get_translations <- function() {
  list(
    app = list(
      language_label = c(
        ENG = "UI language",
        FR = "Langue de l'interface"
      ),
      nav_home = c(
        ENG = "Home",
        FR = "Accueil"
      ),
      nav_interview = c(
        ENG = "Interview simulator",
        FR = "Simuler un entretien"
      ),
      nav_pitch = c(
        ENG = "Improve my pitch",
        FR = "Am\u00e9liorer mon pitch"
      ),
      notify_session_quota = c(
        ENG = "Session usage limit reached (%s/%s API calls). Try again later or start a new session.",
        FR = "Limite d'utilisation de la session atteinte (%s/%s appels API). R\u00e9essayez plus tard ou d\u00e9marrez une nouvelle session."
      ),
      notify_daily_quota = c(
        ENG = "Daily usage limit reached (%s/%s API calls). Please come back tomorrow.",
        FR = "Limite d'utilisation quotidienne atteinte (%s/%s appels API). Revenez demain."
      )
    ),

    home = list(
      hero_eyebrow = c(
        ENG = "GEN-AI \u00b7 Job-search toolkit",
        FR = "GEN-AI \u00b7 Bo\u00eete \u00e0 outils emploi"
      ),
      hero_title = c(
        ENG = "Prepare for your next opportunity with AI",
        FR = "Pr\u00e9parez votre prochaine opportunit\u00e9 avec l'IA"
      ),
      hero_subtitle = c(
        ENG = "A small space to rehearse interviews and work on your pitch \u2014 with an AI that reacts to what you actually say, in French or English.",
        FR = "Un espace pour s'entra\u00eener aux entretiens et travailler son pitch \u2014 avec une IA qui r\u00e9agit \u00e0 ce que vous dites vraiment, en fran\u00e7ais ou en anglais."
      ),
      hero_cta_primary = c(
        ENG = "Start an interview",
        FR = "D\u00e9marrer un entretien"
      ),
      hero_cta_secondary = c(
        ENG = "Improve my pitch",
        FR = "Am\u00e9liorer mon pitch"
      ),
      features_title = c(
        ENG = "What you can do",
        FR = "Ce que vous pouvez faire"
      ),
      feature_interview_title = c(
        ENG = "Interview simulator",
        FR = "Simulateur d'entretien"
      ),
      feature_interview_desc = c(
        ENG = "Practice a realistic interview with an AI recruiter tailored to your role and company, and get live feedback on your answers as you go.",
        FR = "Entra\u00eenez-vous \u00e0 un entretien r\u00e9aliste avec un recruteur IA adapt\u00e9 \u00e0 votre poste et \u00e0 l'entreprise, et recevez un retour en direct sur vos r\u00e9ponses."
      ),
      feature_interview_cta = c(
        ENG = "Open the simulator",
        FR = "Ouvrir le simulateur"
      ),
      feature_pitch_title = c(
        ENG = "Pitch improver",
        FR = "Am\u00e9liorateur de pitch"
      ),
      feature_pitch_desc = c(
        ENG = "Paste your pitch, choose your audience, and get structured feedback on clarity, structure, tone, and the questions you should expect.",
        FR = "Collez votre pitch, choisissez votre interlocuteur et obtenez un retour structur\u00e9 sur la clart\u00e9, la structure, le ton et les questions \u00e0 anticiper."
      ),
      feature_pitch_cta = c(
        ENG = "Open the pitch improver",
        FR = "Ouvrir l'am\u00e9liorateur de pitch"
      ),
      how_title = c(
        ENG = "How to use it",
        FR = "Comment l'utiliser"
      ),
      how_body = c(
        ENG = "Pick a tool from the menu, add a bit of context (role, company, audience), then practice. Feedback shows up as you go \u2014 take what helps, rewrite it in your own words, try again.",
        FR = "Choisissez un outil dans le menu, donnez un peu de contexte (poste, entreprise, interlocuteur), puis entra\u00eenez-vous. Le retour arrive au fil de l'\u00e9change : gardez ce qui vous aide, reformulez avec vos mots, recommencez."
      ),
      stance_title = c(
        ENG = "What this is for",
        FR = "\u00c0 quoi \u00e7a sert vraiment"
      ),
      stance_body = c(
        ENG = "Helping is not doing the work for you. anotheRworkhub is built to make you think about your own words: what you want to say, what matters for the role, what sounds true \u2014 and what does not. The AI holds up a mirror and suggests angles; it does not replace a human expert, and it does not replace your judgment. Keep what resonates. Leave the rest.",
        FR = "Aider, ce n'est pas faire \u00e0 votre place. anotheRworkhub a \u00e9t\u00e9 con\u00e7u pour vous faire r\u00e9fl\u00e9chir \u00e0 votre propre discours : ce que vous voulez dire, ce qui compte pour le poste, ce qui sonne juste \u2014 ou pas. L'IA tend un miroir et propose des pistes ; elle ne remplace ni un regard humain, ni votre esprit critique. Gardez ce qui vous parle. Laissez le reste."
      ),
      tip = c(
        ENG = "Available in French and English \u2014 switch anytime using the toggle at the top right.",
        FR = "Disponible en fran\u00e7ais et en anglais \u2014 changez de langue \u00e0 tout moment gr\u00e2ce au s\u00e9lecteur en haut \u00e0 droite."
      )
    ),

    interview = list(
      hero_title = c(ENG = "Interview Simulator", FR = "Simulateur d'entretien"),
      hero_subtitle = c(
        ENG = "Configure your session on the left, then practice in the live chat.",
        FR = "Configurez votre session \u00e0 gauche, puis entra\u00eenez-vous dans le chat."
      ),
      start = c(ENG = "Start Interview", FR = "D\u00e9marrer l'entretien"),
      restart = c(ENG = "Restart Interview", FR = "Red\u00e9marrer l'entretien"),
      live_chat = c(ENG = "Live interview", FR = "Entretien en direct"),
      chat_empty = c(
        ENG = "Your interview will appear here once you start the session.",
        FR = "Votre entretien appara\u00eetra ici une fois la session d\u00e9marr\u00e9e."
      ),
      chat_welcome = c(
        ENG = "Set up your profile on the left, then click **Start Interview** to begin.",
        FR = "Configurez votre profil \u00e0 gauche, puis cliquez sur **D\u00e9marrer l'entretien**."
      ),
      chat_you = c(ENG = "You", FR = "Vous"),
      chat_interviewer = c(ENG = "Interviewer", FR = "Recruteur"),
      chat_analysis = c(ENG = "Feedback", FR = "Retour"),
      analysis_round = c(ENG = "Feedback \u00b7 Round %s", FR = "Retour \u00b7 \u00c9change %s"),
      analysis_loading = c(
        ENG = "Analyzing your last answers\u2026",
        FR = "Analyse de vos derni\u00e8res r\u00e9ponses\u2026"
      ),
      analysis_in_n = c(
        ENG = "Feedback in %s messages",
        FR = "Retour dans %s messages"
      ),
      analysis_ready = c(ENG = "Feedback updated", FR = "Retour \u00e0 jour"),
      analysis_error = c(
        ENG = "Feedback could not be generated. You can keep interviewing.",
        FR = "Le retour n'a pas pu \u00eatre g\u00e9n\u00e9r\u00e9. Vous pouvez poursuivre l'entretien."
      ),
      toggle_setup_show = c(ENG = "Show settings", FR = "Afficher les param\u00e8tres"),
      toggle_setup_hide = c(ENG = "Hide settings", FR = "Masquer les param\u00e8tres"),
      input_placeholder = c(
        ENG = "Type your answer here...",
        FR = "Tapez votre r\u00e9ponse ici..."
      ),
      send = c(ENG = "Send", FR = "Envoyer"),
      assistant_typing = c(
        ENG = "The interviewer is preparing a reply\u2026",
        FR = "Le recruteur pr\u00e9pare sa r\u00e9ponse\u2026"
      ),
      progress = list(
        ENG = c("Profile", "Format", "Company", "Role"),
        FR = c("Profil", "Format", "Entreprise", "Poste")
      ),
      notify_short_message = c(
        ENG = "Message is too short!",
        FR = "Le message est trop court !"
      ),
      notify_fill_required = c(
        ENG = "Please fill in all required fields properly",
        FR = "Veuillez remplir tous les champs requis correctement"
      ),
      notify_api_key_missing = c(
        ENG = "OpenAI API key missing. Set API_KEY in .Renviron.",
        FR = "Cl\u00e9 API OpenAI manquante. D\u00e9finissez API_KEY dans .Renviron."
      ),
      notify_context_error = c(
        ENG = "Unable to build interview context.",
        FR = "Impossible de construire le contexte d'entretien."
      ),
      notify_error = c(ENG = "Error: %s", FR = "Erreur : %s"),
      notify_cooldown = c(
        ENG = "Please wait %s seconds before sending another message.",
        FR = "Veuillez attendre %s secondes avant d'envoyer un nouveau message."
      ),
      notify_session_quota = c(
        ENG = "Session usage limit reached (%s/%s API calls).",
        FR = "Limite d'utilisation de la session atteinte (%s/%s appels API)."
      ),
      notify_daily_quota = c(
        ENG = "Daily usage limit reached (%s/%s API calls).",
        FR = "Limite d'utilisation quotidienne atteinte (%s/%s appels API)."
      ),
      notify_interview_message_quota = c(
        ENG = "Interview message limit reached (%s/%s). Restart to begin a new session.",
        FR = "Limite de messages d'entretien atteinte (%s/%s). Red\u00e9marrez pour une nouvelle session."
      ),
      notify_message_too_long = c(
        ENG = "Message is too long (max %s characters).",
        FR = "Le message est trop long (max %s caract\u00e8res)."
      ),
      progress_start = c(ENG = "Starting conversation...", FR = "D\u00e9marrage de la conversation..."),
      progress_processing = c(ENG = "Processing...", FR = "Traitement..."),
      gpt_language_label = c(
        ENG = "Language for AI responses",
        FR = "Langue des r\u00e9ponses IA"
      ),
      gpt_language_choices = list(
        ENG = c("French" = "FR", "English" = "ENG"),
        FR = c("Fran\u00e7ais" = "FR", "Anglais" = "ENG")
      ),
      labels = list(
        candidate_name = c(ENG = "Candidate name", FR = "Nom du candidat"),
        position = c(ENG = "Position", FR = "Poste"),
        company_sector = c(ENG = "Industry sector", FR = "Secteur d'activit\u00e9"),
        basic_settings_title = c(ENG = "Basic information", FR = "Informations de base"),
        interview_format_title = c(ENG = "Interview format", FR = "Format d'entretien"),
        company_info_title = c(ENG = "Company information", FR = "Information sur l'entreprise"),
        job_details_title = c(ENG = "Job details", FR = "D\u00e9tails du poste"),
        format_label = c(ENG = "Interview format", FR = "Format d'entretien"),
        criteria_label = c(ENG = "Assessment criteria", FR = "Crit\u00e8res d'\u00e9valuation"),
        time_label = c(ENG = "Time constraints", FR = "Contraintes de temps"),
        follow_up_label = c(ENG = "Follow-up process", FR = "Processus de suivi"),
        culture_label = c(ENG = "Company culture", FR = "Culture d'entreprise"),
        values_label = c(ENG = "Company values", FR = "Valeurs de l'entreprise"),
        challenges_label = c(ENG = "Business challenges", FR = "D\u00e9fis commerciaux"),
        growth_label = c(ENG = "Growth phase", FR = "Phase de croissance"),
        reputation_label = c(ENG = "Company reputation", FR = "R\u00e9putation de l'entreprise"),
        environment_label = c(ENG = "Work environment", FR = "Environnement de travail"),
        responsibilities_label = c(ENG = "Key responsibilities", FR = "Responsabilit\u00e9s principales"),
        skills_label = c(ENG = "Required skills", FR = "Comp\u00e9tences requises"),
        team_label = c(ENG = "Team structure", FR = "Structure de l'\u00e9quipe"),
        job_challenges_label = c(ENG = "Key challenges", FR = "D\u00e9fis principaux"),
        performance_label = c(ENG = "Performance expectations", FR = "Attentes de performance"),
        career_label = c(ENG = "Career development", FR = "D\u00e9veloppement de carri\u00e8re")
      ),
      placeholders = list(
        name = c(ENG = "Enter candidate's name...", FR = "Entrez le nom du candidat..."),
        job_title = c(ENG = "Enter job title...", FR = "Entrez l'intitul\u00e9 du poste...")
      ),
      choices = list(
        company_sector = list(
          ENG = c(
            "Agriculture, Forestry, and Fishing",
            "Mining and Quarrying",
            "Manufacturing",
            "Electricity, Gas, Steam, and Air Conditioning Supply",
            "Water Supply, Sewerage, Waste Management, and Remediation Activities",
            "Construction",
            "Wholesale and Retail Trade; Repair of Motor Vehicles and Motorcycles",
            "Transportation and Storage",
            "Accommodation and Food Service Activities",
            "Information and Communication",
            "Financial and Insurance Activities",
            "Real Estate Activities",
            "Professional, Scientific, and Technical Activities",
            "Administrative and Support Service Activities",
            "Public Administration and Defence; Compulsory Social Security",
            "Education",
            "Human Health and Social Work Activities",
            "Arts, Entertainment, and Recreation",
            "Other Service Activities"
          ),
          FR = c(
            "Agriculture, Sylviculture et P\u00eache",
            "Extraction Mini\u00e8re et Carri\u00e8res",
            "Industrie Manufacturi\u00e8re",
            "Production et Distribution d'\u00c9lectricit\u00e9, de Gaz, de Vapeur et de Climatisation",
            "Captage, Traitement et Distribution d'Eau; Assainissement, Gestion des D\u00e9chets",
            "Construction",
            "Commerce de Gros et de D\u00e9tail; R\u00e9paration de V\u00e9hicules Automobiles et de Motocycles",
            "Transport et Entreposage",
            "H\u00e9bergement et Services de Restauration",
            "Information et Communication",
            "Activit\u00e9s Financi\u00e8res et d'Assurance",
            "Activit\u00e9s Immobili\u00e8res",
            "Activit\u00e9s Professionnelles, Scientifiques et Techniques",
            "Activit\u00e9s de Services Administratifs et de Soutien",
            "Administration Publique et D\u00e9fense; S\u00e9curit\u00e9 Sociale Obligatoire",
            "\u00c9ducation",
            "Activit\u00e9s de Sant\u00e9 Humaine et d'Action Sociale",
            "Arts, Spectacles et Activit\u00e9s R\u00e9cr\u00e9atives",
            "Autres Activit\u00e9s de Services"
          )
        ),
        interview_format = list(
          ENG = c("", "First interview", "Follow-up interview", "Case study", "Technical"),
          FR = c("", "Premier entretien", "Entretien de suivi", "\u00c9tude de cas", "Technique")
        ),
        assessment_criteria = list(
          ENG = c("", "Technical skills", "Soft skills", "Leadership", "Problem solving"),
          FR = c("", "Comp\u00e9tences techniques", "Comp\u00e9tences relationnelles", "Leadership", "R\u00e9solution de probl\u00e8mes")
        ),
        time_constraints = list(
          ENG = c("", "5mn", "10mn", "20mn"),
          FR = c("", "5mn", "10mn", "20mn")
        ),
        follow_up_process = list(
          ENG = c("", "Same day", "Within week", "Multiple rounds"),
          FR = c("", "M\u00eame jour", "Dans la semaine", "Plusieurs tours")
        ),
        company_culture = list(
          ENG = c("", "Formal", "Casual", "Startup", "Corporate"),
          FR = c("", "Formel", "D\u00e9contract\u00e9", "Startup", "Entreprise")
        ),
        growth_phase = list(
          ENG = c("", "Startup", "Growth", "Mature", "Transformation"),
          FR = c("", "D\u00e9marrage", "Croissance", "Mature", "Transformation")
        ),
        work_environment = list(
          ENG = c("", "Remote", "Hybrid", "Office-based", "Flexible"),
          FR = c("", "T\u00e9l\u00e9travail", "Hybride", "Pr\u00e9sentiel", "Flexible")
        )
      )
    ),

    pitch = list(
      hero_title = c(ENG = "Pitch improver", FR = "Am\u00e9liorer mon pitch"),
      hero_subtitle = c(
        ENG = "Craft your pitch, pick your audience, and get AI feedback tailored to the context.",
        FR = "R\u00e9digez votre pitch, choisissez votre interlocuteur et obtenez un retour IA adapt\u00e9 au contexte."
      ),
      required_fields = c(ENG = "Required fields", FR = "Champs requis"),
      context_label = c(
        ENG = "What is the context of the exchange?",
        FR = "Quel est le contexte de l'\u00e9change ?"
      ),
      recipient_label = c(
        ENG = "Who is your pitch addressed to?",
        FR = "\u00c0 qui est adress\u00e9 votre pitch ?"
      ),
      hierarchical_label = c(
        ENG = "Recipient's hierarchical status",
        FR = "Statut hi\u00e9rarchique du destinataire"
      ),
      optional_toggle = c(ENG = "Additional details", FR = "D\u00e9tails suppl\u00e9mentaires"),
      background_label = c(
        ENG = "Recipient background / personality",
        FR = "Parcours ou personnalit\u00e9 du destinataire"
      ),
      activity_label = c(
        ENG = "Recipient's industry sector",
        FR = "Secteur d'activit\u00e9 du destinataire"
      ),
      expertise_label = c(
        ENG = "Recipient's expertise",
        FR = "Expertise du destinataire"
      ),
      gpt_language_label = c(
        ENG = "Language for AI responses",
        FR = "Langue des r\u00e9ponses IA"
      ),
      expectations_label = c(ENG = "Level of expectations", FR = "Niveau d'exigence"),
      pitch_label = c(ENG = "Pitch to analyze", FR = "Pitch \u00e0 analyser"),
      pitch_default = c(
        ENG = paste(
          "Hi, I'm Alex, a product analyst with five years of experience in B2B SaaS.",
          "I turn complex customer data into clear product decisions.",
          "Recently I led a project that cut onboarding time by 30%.",
          "I'd love to discuss how I could help your team ship insights faster \u2014",
          "would you be open to a short call next week?"
        ),
        FR = paste(
          "Bonjour, je suis Alex, analyste produit avec cinq ans d'exp\u00e9rience dans le SaaS B2B.",
          "Je transforme des donn\u00e9es clients complexes en d\u00e9cisions produit claires.",
          "R\u00e9cemment, j'ai pilot\u00e9 un projet qui a r\u00e9duit le temps d'onboarding de 30 %.",
          "J'aimerais \u00e9changer sur la fa\u00e7on dont je pourrais aider votre \u00e9quipe \u2014",
          "seriez-vous disponible pour un court appel la semaine prochaine ?"
        )
      ),
      char_progress_label = c(ENG = "Pitch length", FR = "Longueur du pitch"),
      analyze_button = c(ENG = "Start analysis", FR = "D\u00e9marrer l'analyse"),
      validation_message = c(
        ENG = "Please fill in all required fields and enter a pitch with more than 100 characters.",
        FR = "Veuillez remplir tous les champs obligatoires et entrer un pitch de plus de 100 caract\u00e8res."
      ),
      notify_session_quota = c(
        ENG = "Session usage limit reached (%s/%s API calls).",
        FR = "Limite d'utilisation de la session atteinte (%s/%s appels API)."
      ),
      notify_daily_quota = c(
        ENG = "Daily usage limit reached (%s/%s API calls).",
        FR = "Limite d'utilisation quotidienne atteinte (%s/%s appels API)."
      ),
      notify_pitch_quota = c(
        ENG = "Pitch analysis limit reached (%s/%s). Try again later.",
        FR = "Limite d'analyses de pitch atteinte (%s/%s). R\u00e9essayez plus tard."
      ),
      notify_pitch_cooldown = c(
        ENG = "Please wait %s seconds before another pitch analysis.",
        FR = "Veuillez attendre %s secondes avant une nouvelle analyse de pitch."
      ),
      notify_pitch_in_flight = c(
        ENG = "A pitch analysis is already running.",
        FR = "Une analyse de pitch est d\u00e9j\u00e0 en cours."
      ),
      notify_pitch_too_long = c(
        ENG = "Pitch is too long (max %s characters).",
        FR = "Le pitch est trop long (max %s caract\u00e8res)."
      ),
      notify_api_key_missing = c(
        ENG = "OpenAI API key missing. Set API_KEY in .Renviron.",
        FR = "Cl\u00e9 API OpenAI manquante. D\u00e9finissez API_KEY dans .Renviron."
      ),
      unknown_choice = c(ENG = "Unknown choice", FR = "Choix inconnu"),
      recipient_placeholder = c(
        ENG = "Enter recipient name...",
        FR = "Entrez le nom du destinataire..."
      ),
      analysis_orthography = c(ENG = "Orthography & grammar", FR = "Orthographe & grammaire"),
      analysis_structure = c(ENG = "Structure & coherence", FR = "Structure & coh\u00e9rence"),
      analysis_questions = c(ENG = "Potential questions", FR = "Questions \u00e9ventuelles"),
      analysis_sentiment = c(ENG = "Emotional valence", FR = "Valence \u00e9motionnelle"),
      modal_orthography = c(ENG = "Analyzing orthography and grammar...", FR = "Analyse de l'orthographe et de la grammaire..."),
      modal_structure = c(ENG = "Analyzing structure...", FR = "Analyse de la structure..."),
      modal_questions = c(ENG = "Generating potential questions...", FR = "G\u00e9n\u00e9ration des questions \u00e9ventuelles..."),
      modal_sentiment = c(ENG = "Analyzing emotional valence...", FR = "Analyse de la valence \u00e9motionnelle..."),
      recap_empty = c(
        ENG = "No prompt summary available yet.",
        FR = "Aucun r\u00e9capitulatif disponible pour le moment."
      ),
      stats_labels = list(
        ENG = c("Characters", "Words", "Sentences", "Paragraphs"),
        FR = c("Caract\u00e8res", "Mots", "Phrases", "Paragraphes")
      ),
      expectations_levels = list(
        ENG = list(
          list(value = "High", label = "High", desc = "Standard excellence"),
          list(value = "Very high", label = "Very high", desc = "Superior quality"),
          list(value = "Exceptionally High Expectations", label = "Exceptional", desc = "Outstanding performance")
        ),
        FR = list(
          list(value = "Elev\u00e9", label = "\u00c9lev\u00e9", desc = "Excellence standard"),
          list(value = "Tr\u00e8s \u00e9lev\u00e9", label = "Tr\u00e8s \u00e9lev\u00e9", desc = "Qualit\u00e9 sup\u00e9rieure"),
          list(value = "Exceptionnellement \u00e9lev\u00e9", label = "Exceptionnel", desc = "Performance exceptionnelle")
        )
      ),
      hierarchical_roles = list(
        ENG = list(
          entry_level = list(icon = "user", label = "Entry level"),
          manager = list(icon = "user-tie", label = "Manager"),
          senior_manager = list(icon = "users", label = "Senior manager"),
          director = list(icon = "user-graduate", label = "Director"),
          ceo = list(icon = "crown", label = "CEO")
        ),
        FR = list(
          entry_level = list(icon = "user", label = "D\u00e9butant"),
          manager = list(icon = "user-tie", label = "Manager"),
          senior_manager = list(icon = "users", label = "Manager senior"),
          director = list(icon = "user-graduate", label = "Directeur"),
          ceo = list(icon = "crown", label = "PDG")
        )
      ),
      gpt_language_choices = list(
        ENG = c("French" = "FR", "English" = "ENG"),
        FR = c("Fran\u00e7ais" = "FR", "Anglais" = "ENG")
      )
    ),

    english_choices_map = c(
      "spontaneous_application" = "Spontaneous application",
      "offer_reply" = "Reply to an offer",
      "phone_screening" = "Phone screening",
      "first_formal_meeting" = "Formal meeting",
      "first_informal_meeting" = "Informal meeting",
      "networking_event" = "Networking event",
      "followup_after_networking" = "Follow-up after networking",
      "one_on_one_interview" = "One-on-one interview",
      "job_offer_acceptance" = "Job offer acceptance",
      "job_offer_clarification" = "Job offer clarification",
      "rejecting_job_offer" = "Rejecting a job offer",
      "asking_for_feedback" = "Asking for feedback",
      "followup_after_interview" = "Follow-up after interview"
    ),

    french_choices_map = c(
      "spontaneous_application" = "Candidature spontan\u00e9e",
      "offer_reply" = "R\u00e9ponse \u00e0 une offre",
      "phone_screening" = "Entretien t\u00e9l\u00e9phonique",
      "first_formal_meeting" = "R\u00e9union formelle",
      "first_informal_meeting" = "R\u00e9union informelle",
      "networking_event" = "\u00c9v\u00e9nement de r\u00e9seautage",
      "followup_after_networking" = "Suivi apr\u00e8s r\u00e9seautage",
      "one_on_one_interview" = "Entretien individuel",
      "job_offer_acceptance" = "Acceptation d'offre d'emploi",
      "job_offer_clarification" = "Clarification d'offre d'emploi",
      "rejecting_job_offer" = "Refus d'offre d'emploi",
      "asking_for_feedback" = "Demande de retour",
      "followup_after_interview" = "Suivi apr\u00e8s entretien"
    ),

    hierarchical_status_labels_en = c(
      "entry_level" = "Entry level / junior",
      "manager" = "Manager / supervisor",
      "senior_manager" = "Senior manager / department head",
      "director" = "Director / vice president",
      "ceo" = "CEO / executive"
    ),

    hierarchical_status_labels_fr = c(
      "entry_level" = "Niveau d'entr\u00e9e / d\u00e9butant",
      "manager" = "Manager / superviseur",
      "senior_manager" = "Manager senior / chef de d\u00e9partement",
      "director" = "Directeur / vice-pr\u00e9sident",
      "ceo" = "PDG / ex\u00e9cutif"
    ),

    expect_choices_en = c(
      "High" = "High",
      "Very high" = "Very high",
      "Exceptionally High Expectations" = "Exceptionally high expectations"
    ),

    expect_choices_fr = c(
      "Elev\u00e9" = "\u00c9lev\u00e9",
      "Tr\u00e8s \u00e9lev\u00e9" = "Tr\u00e8s \u00e9lev\u00e9",
      "Exceptionnellement \u00e9lev\u00e9" = "Exceptionnellement \u00e9lev\u00e9"
    )
  )
}
