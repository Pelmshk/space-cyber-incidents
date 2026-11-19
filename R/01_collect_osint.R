# ============================================================================
# R/01_collect_osint.R
# Сбор OSINT-данных об инцидентах ИБ в космических системах
# ============================================================================
# Источники:
#   1. RSS: 25 фидов (космос, кибербез, OT/ICS)
#   2. GDELT: 12 паттернов, диапазон 3 года
#   3. CISA: RSS всех advisory + ICS + KEV
#   4. NASA: RSS + HTML пагинация + ESA
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(rvest)
  library(jsonlite)
  library(tidyRSS)
  library(lubridate)
  library(readr)
  library(purrr)
  library(cli)
})

# ----------------------------------------------------------------------------
# 0. Конфигурация
# ----------------------------------------------------------------------------

SNAPSHOT_DATE <- Sys.Date()
SNAPSHOT_TS   <- format(Sys.time(), "%Y%m%d_%H%M%S")

RAW_DIR <- "data/raw"
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RAW_DIR, "rss"), showWarnings = FALSE)
dir.create(file.path(RAW_DIR, "gdelt"), showWarnings = FALSE)
dir.create(file.path(RAW_DIR, "cisa"), showWarnings = FALSE)
dir.create(file.path(RAW_DIR, "nasa"), showWarnings = FALSE)
dir.create(file.path(RAW_DIR, "meta"), showWarnings = FALSE)

# 12 паттернов GDELT, диапазон 3 года
SPACE_CYBER_PATTERNS <- c(
  "satellite hack OR satellite cyberattack OR satellite breach",
  "GPS jamming OR GNSS spoofing OR satellite jamming",
  "ground station cyber OR satellite control hack",
  "space supply chain OR satellite vendor compromise",
  "anti-satellite weapon OR ASAT OR space debris",
  "space warfare OR satellite military attack",
  "Viasat hack OR Starlink cyber OR satellite disruption",
  "space system vulnerability OR satellite malware",
  "cyberattack satellite OR satellite data breach",
  "satellite network intrusion OR space network hack",
  "GNSS interference OR GPS spoofing attack",
  "satellite command hijack OR telemetry spoofing"
)

# 25 RSS-фидов
RSS_FEEDS <- tibble(
  feed_name = c(
    "SpaceNews", "SpaceflightNow", "SpaceSafetyMagazine",
    "SpaceNews_Europe", "ParabolicArc", "SpaceWatchGlobal",
    "ArsTechnica_Space", "TheRegister_Science",
    "SpaceIntelReport", "SpacePolicyOnline",
    "TheHackerNews", "BleepingComputer", "KrebsOnSecurity",
    "DarkReading", "SecurityWeek", "TheRecord",
    "ThreatPost", "CyberScoop",
    "ControlGlobal", "DefenseNews", "BreakingDefense",
    "SatNews", "FlightGlobal", "AviationWeek", "JanesDefence"
  ),
  feed_url = c(
    "https://spacenews.com/feed/",
    "https://spaceflightnow.com/feed/",
    "https://www.spacesafetymagazine.com/feed/",
    "https://spacenews.com/section/europe/feed/",
    "https://parabolicarc.com/feed/",
    "https://spacewatch.global/feed/",
    "https://arstechnica.com/space/feed/",
    "https://www.theregister.com/headlines.atom",
    "https://spaceintelreport.com/feed/",
    "https://spacepolicyonline.com/feed",
    "https://feeds.feedburner.com/TheHackersNews",
    "https://www.bleepingcomputer.com/feed/",
    "https://krebsonsecurity.com/feed/",
    "https://www.darkreading.com/rss.xml",
    "https://www.securityweek.com/feed/",
    "https://therecord.media/feed",
    "https://threatpost.com/feed/",
    "https://www.cyberscoop.com/feed/",
    "https://www.controlglobal.com/rss",
    "https://www.defensenews.com/arc/outboundfeeds/rss/?outputType=xml",
    "https://breakingdefense.com/feed/",
    "https://www.satnews.com/rss.xml",
    "https://www.flightglobal.com/rss.xml",
    "https://aviationweek.com/rss.xml",
    "https://www.janes.com/rss"
  ),
  feed_category = c(
    rep("space", 10),
    rep("cyber", 8),
    rep("ics_defense", 7)
  )
)

log_step <- function(step, status = "info", msg = "") {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  switch(
    status,
    "info"    = cli::cli_alert_info("[{ts}] {step}: {msg}"),
    "warn"    = cli::cli_alert_warning("[{ts}] {step}: {msg}"),
    "success" = cli::cli_alert_success("[{ts}] {step}: {msg}"),
    "error"   = cli::cli_alert_danger("[{ts}] {step}: {msg}"),
    cli::cli_alert("[{ts}] {step}: {msg}")
  )
}

# ----------------------------------------------------------------------------
# 1. Сбор RSS
# ----------------------------------------------------------------------------

collect_rss <- function(feeds_tbl, snapshot_date) {
  log_step("RSS", "start", sprintf("Парсинг %d RSS-фидов", nrow(feeds_tbl)))
  
  parse_feed <- function(url) {
    res <- safely(tidyRSS::tidyfeed)(url)
    
    if (!is.null(res$error) || is.null(res$result) || nrow(res$result) == 0) {
      return(tibble(
        title = character(), link = character(),
        pub_date = character(), description = character()
      ))
    }
    
    df <- res$result
    
    # Универсальное извлечение полей (работает со всеми версиями tidyRSS)
    get_col <- function(names) {
      for (n in names) {
        if (n %in% names(df)) return(as.character(df[[n]]))
      }
      rep(NA_character_, nrow(df))
    }
    
    tibble(
      title       = get_col(c("item_title", "title")),
      link        = get_col(c("item_link", "link")),
      pub_date    = get_col(c("item_pubDate", "pubDate", "published", "dc:date")),
      description = get_col(c("item_description", "description", "summary"))
    )
  }
  
  results <- feeds_tbl |>
    mutate(data = map(feed_url, parse_feed)) |>
    unnest(data) |>
    mutate(
      snapshot_date = snapshot_date,
      feed_source   = feed_name
    )
  
  out_path <- file.path(RAW_DIR, "rss", sprintf("rss_raw_%s.rds", SNAPSHOT_TS))
  write_rds(results, out_path)
  log_step("RSS", "success", sprintf("Сохранено %d записей", nrow(results)))
  invisible(results)
}

# ----------------------------------------------------------------------------
# 2. GDELT API
# ----------------------------------------------------------------------------

collect_gdelt <- function(patterns, snapshot_date, max_records = 100) {
  log_step("GDELT", "start", sprintf("%d паттернов, диапазон 3 года", length(patterns)))
  base_url <- "https://api.gdeltproject.org/api/v2/doc/doc"
  
  empty_gdelt_tibble <- function() {
    tibble(
      title = character(), url = character(),
      feed_source = character(), date = character(),
      language = character(), country = character(),
      query = character(), snapshot = as.Date(character())
    )
  }
  
  fetch_pattern <- function(pattern, idx, total) {
    cat(sprintf("  -> Паттерн %d/%d: '%s'\n", idx, total, str_sub(pattern, 1, 50)))
    
    try_fetch <- function(attempt = 1, max_attempts = 3) {
      req <- request(base_url) |>
        req_url_query(
          query = pattern, mode = "ArtList", format = "json",
          maxrecords = max_records,
          startdate = format(snapshot_date - years(3), "%Y%m%dT000000Z"),
          enddate = format(snapshot_date, "%Y%m%dT235959Z"),
          sort = "DateDesc"
        ) |>
        req_headers(
          `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/124.0.0.0",
          `Accept` = "application/json"
        ) |>
        req_timeout(30)
      
      resp <- tryCatch(
        req_perform(req),
        error = function(e) {
          if (attempt < max_attempts && grepl("429", e$message)) {
            wait_time <- 30 * attempt
            cat(sprintf("     429 - повтор через %d сек (попытка %d)\n", wait_time, attempt + 1))
            Sys.sleep(wait_time)
            return(try_fetch(attempt + 1, max_attempts))
          }
          log_step("GDELT", "warn", sprintf("Ошибка '%s': %s", str_sub(pattern, 1, 30), e$message))
          NULL
        }
      )
      
      if (!is.null(resp)) {
        content_type <- resp_content_type(resp)
        if (!grepl("json", content_type, ignore.case = TRUE)) {
          log_step("GDELT", "warn", sprintf("Паттерн '%s' вернул %s вместо JSON",
                                            str_sub(pattern, 1, 30), content_type))
          return(NULL)
        }
      }
      
      resp
    }
    
    resp <- try_fetch()
    if (is.null(resp)) return(empty_gdelt_tibble())
    
    body <- tryCatch(
      resp_body_json(resp, simplifyVector = TRUE),
      error = function(e) {
        log_step("GDELT", "warn", sprintf("Не удалось распарсить JSON: %s", e$message))
        NULL
      }
    )
    
    if (is.null(body)) return(empty_gdelt_tibble())
    
    articles <- body$articles
    if (is.null(articles) || length(articles) == 0) {
      cat("    - 0 статей\n"); return(empty_gdelt_tibble())
    }
    cat(sprintf("    - %d статей\n", length(articles$title)))
    
    tibble(
      title = articles$title %||% NA_character_,
      url = articles$url %||% NA_character_,
      feed_source = articles$domain %||% NA_character_,
      date = articles$seendate %||% NA_character_,
      language = articles$language %||% NA_character_,
      country = articles$sourcecountry %||% NA_character_,
      query = pattern,
      snapshot = snapshot_date
    )
  }
  
  results <- map2_dfr(
    patterns, seq_along(patterns),
    ~ {
      if (.y > 1) { cat("  - 12 сек...\n"); Sys.sleep(12) }
      fetch_pattern(.x, .y, length(patterns))
    }
  ) |> distinct(url, .keep_all = TRUE)
  
  out_path <- file.path(RAW_DIR, "gdelt", sprintf("gdelt_raw_%s.rds", SNAPSHOT_TS))
  write_rds(results, out_path)
  log_step("GDELT", "success", sprintf("%d уникальных статей", nrow(results)))
  invisible(results)
}

# ----------------------------------------------------------------------------
# 3. CISA (RSS + HTML пагинация + KEV)
# ----------------------------------------------------------------------------

collect_cisa <- function(snapshot_date) {
  log_step("CISA", "start", "Парсинг CISA (RSS + HTML + KEV)")
  
  # 1. RSS-фиды CISA
  cisa_rss_urls <- c(
    "https://www.cisa.gov/cybersecurity-advisories/all.xml",
    "https://www.cisa.gov/news-events/ics-advisories/feed",
    "https://www.cisa.gov/news-events/alerts/feed",
    "https://www.cisa.gov/uscert/ncas/alerts.xml",
    "https://www.cisa.gov/uscert/ncas/analysis-reports.xml",
    "https://www.cisa.gov/uscert/ncas/current-activity.xml",
    "https://www.cisa.gov/news-events/cybersecurity-advisories/feed",
    "https://www.cisa.gov/news-events/news/feed",
    "https://www.cisa.gov/ics-cert/advisories/feed",
    "https://www.cisa.gov/uscert/ncas/tips/feed"
  )
  
  extract_feed_cols <- function(df, feed_name) {
    if (nrow(df) == 0) return(tibble())
    
    title <- if ("item_title" %in% names(df)) df$item_title else
      if ("title" %in% names(df)) df$title else NA_character_
    link <- if ("item_link" %in% names(df)) df$item_link else
      if ("link" %in% names(df)) df$link else NA_character_
    pub_date <- if ("item_pubDate" %in% names(df)) df$item_pubDate else
      if ("pub_date" %in% names(df)) df$pub_date else
        if ("pubDate" %in% names(df)) df$pubDate else NA_character_
    
    tibble(
      title = as.character(title),
      url = as.character(link),
      pub_date = as.character(pub_date),
      feed = feed_name
    )
  }
  
  rss_items <- map_dfr(cisa_rss_urls, function(u) {
    res <- safely(tidyRSS::tidyfeed)(u)$result %||% tibble()
    extract_feed_cols(res, basename(dirname(u)))
  })
  
  # 2. HTML-парсинг с пагинацией (10 страниц)
  html_items <- tibble()
  base_url <- "https://www.cisa.gov/news-events/news"
  
  for (page_num in 1:10) {
    url <- if (page_num == 1) base_url else paste0(base_url, "?page=", page_num - 1)
    
    req <- request(url) |>
      req_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
        `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
      ) |>
      req_timeout(20)
    
    resp <- tryCatch(req_perform(req), error = function(e) NULL)
    if (is.null(resp)) next
    
    page <- httr2::resp_body_html(resp)
    nodes <- html_elements(page, ".news-item a, .views-row a, article a, .field--name-title a, .teaser__title a") |>
      html_elements("a")
    
    if (length(nodes) == 0) next
    
    page_items <- tibble(
      title = html_text2(nodes),
      url = html_attr(nodes, "href")
    ) |>
      filter(!is.na(title), !is.na(url), str_length(title) > 5) |>
      mutate(url = ifelse(str_starts(url, "/"), paste0("https://www.cisa.gov", url), url))
    
    html_items <- bind_rows(html_items, page_items)
    Sys.sleep(2)
  }
  
  # 3. KEV (Known Exploited Vulnerabilities)
  kev_items <- tryCatch({
    req <- request("https://www.cisa.gov/sites/default/files/feeds/known_exploited_vulnerabilities.json") |>
      req_headers(`User-Agent` = "Mozilla/5.0") |>
      req_timeout(20)
    resp <- req_perform(req)
    body <- resp_body_json(resp, simplifyVector = TRUE)
    
    if (!is.null(body$vulnerabilities)) {
      tibble(
        title = paste0("[KEV] ", body$vulnerabilities$cveID, ": ", body$vulnerabilities$shortDescription),
        url = paste0("https://nvd.nist.gov/vuln/detail/", body$vulnerabilities$cveID),
        pub_date = NA_character_,
        feed = "KEV"
      ) |>
        filter(str_detect(tolower(title), regex("satellite|space|ics|scada|gps|gnss|aerospace|cyber|infrastructure", ignore_case = TRUE)))
    } else {
      tibble()
    }
  }, error = function(e) tibble())
  
  # Объединяем
  all_items <- bind_rows(
    rss_items |> mutate(feed_source = "CISA_RSS"),
    html_items |> mutate(feed_source = "CISA_HTML", pub_date = NA_character_),
    kev_items |> mutate(feed_source = "CISA_KEV")
  ) |>
    filter(!is.na(title), !is.na(url)) |>
    mutate(
      source = "CISA",
      snapshot = snapshot_date
    ) |>
    filter(str_detect(tolower(title), regex(
      "satellite|space|ics|scada|gps|gnss|aerospace|cyber|vulnerability|advisory|alert|critical infrastructure|ot|operational technology|uav|drone|rocket|launch|lockheed|boeing|spacex|northrop|raytheon|l3harris|arianespace|defense|military|national security|exploit|cve|patch|update|security|breach|hack|malware|ransomware|phishing|ddos",
      ignore_case = TRUE
    ))) |>
    distinct(url, .keep_all = TRUE)
  
  out_path <- file.path(RAW_DIR, "cisa", sprintf("cisa_raw_%s.rds", SNAPSHOT_TS))
  write_rds(all_items, out_path)
  log_step("CISA", "success", sprintf("Сохранено %d записей", nrow(all_items)))
  invisible(all_items)
}

# ----------------------------------------------------------------------------
# 4. NASA (RSS + HTML пагинация + ESA)
# ----------------------------------------------------------------------------

collect_nasa <- function(snapshot_date) {
  log_step("NASA", "start", "Парсинг NASA (RSS + HTML + ESA)")
  
  # 1. RSS-фиды NASA + ESA
  nasa_rss_urls <- c(
    "https://www.nasa.gov/news-release/feed/",
    "https://blogs.nasa.gov/safety/feed/",
    "https://blogs.nasa.gov/spacestation/feed/",
    "https://blogs.nasa.gov/commercialcrew/feed/",
    "https://blogs.nasa.gov/artemis/feed/",
    "https://www.nasa.gov/centers-and-facilities/langley/feed/",
    "https://www.nasa.gov/centers-and-facilities/goddard/feed/",
    "https://www.nasa.gov/centers-and-facilities/jpl/feed/",
    "https://www.nasa.gov/centers-and-facilities/kennedy/feed/",
    "https://www.nasa.gov/centers-and-facilities/marshall/feed/",
    "https://www.esa.int/rss/Space_Safety",
    "https://www.esa.int/rss/Human_Spaceflight",
    "https://www.esa.int/rss/Earth_Observation",
    "https://global.jaxa.jp/press/atom.xml",
    "https://www.roscosmos.ru/rss/"
  )
  
  extract_feed_cols <- function(df, feed_name) {
    if (nrow(df) == 0) return(tibble())
    
    title <- if ("item_title" %in% names(df)) df$item_title else
      if ("title" %in% names(df)) df$title else NA_character_
    link <- if ("item_link" %in% names(df)) df$item_link else
      if ("link" %in% names(df)) df$link else NA_character_
    pub_date <- if ("item_pubDate" %in% names(df)) df$item_pubDate else
      if ("pub_date" %in% names(df)) df$pub_date else
        if ("pubDate" %in% names(df)) df$pubDate else NA_character_
    
    tibble(
      title = as.character(title),
      url = as.character(link),
      pub_date = as.character(pub_date),
      feed = feed_name
    )
  }
  
  rss_items <- map_dfr(nasa_rss_urls, function(u) {
    res <- safely(tidyRSS::tidyfeed)(u)$result %||% tibble()
    extract_feed_cols(res, basename(dirname(u)))
  })
  
  # 2. HTML-парсинг NASA с пагинацией (10 страниц)
  html_items <- tibble()
  base_url <- "https://www.nasa.gov/news/all-news/"
  
  for (page_num in 1:10) {
    url <- if (page_num == 1) base_url else paste0(base_url, "?page=", page_num - 1)
    
    req <- request(url) |>
      req_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36") |>
      req_timeout(20)
    
    resp <- tryCatch(req_perform(req), error = function(e) NULL)
    if (is.null(resp)) next
    
    page <- httr2::resp_body_html(resp)
    nodes <- html_elements(page, "h3 a, .card-title a, article a, .teaser__title a, .news-item a")
    
    if (length(nodes) == 0) next
    
    page_items <- tibble(
      title = html_text2(nodes),
      url = html_attr(nodes, "href")
    ) |>
      filter(!is.na(title), str_length(title) > 10) |>
      mutate(url = ifelse(str_starts(url, "/"), paste0("https://www.nasa.gov", url), url))
    
    html_items <- bind_rows(html_items, page_items)
    Sys.sleep(2)
  }
  
  # 3. ESA HTML-парсинг
  esa_items <- tryCatch({
    req <- request("https://www.esa.int/Science_Exploration/Space_Safety") |>
      req_headers(`User-Agent` = "Mozilla/5.0") |>
      req_timeout(20)
    resp <- req_perform(req)
    page <- httr2::resp_body_html(resp)
    
    nodes <- html_elements(page, "a")
    tibble(
      title = html_text2(nodes),
      url = html_attr(nodes, "href")
    ) |>
      filter(!is.na(title), str_length(title) > 10, str_starts(url, "https://www.esa.int")) |>
      mutate(url = ifelse(str_starts(url, "/"), paste0("https://www.esa.int", url), url))
  }, error = function(e) tibble())
  
  # Объединяем
  all_items <- bind_rows(
    rss_items |> mutate(feed_source = "NASA_RSS"),
    html_items |> mutate(feed_source = "NASA_HTML", pub_date = NA_character_),
    esa_items |> mutate(feed_source = "ESA_HTML", pub_date = NA_character_)
  ) |>
    filter(!is.na(title), !is.na(url)) |>
    mutate(
      source = "NASA",
      snapshot = snapshot_date
    ) |>
    filter(str_detect(tolower(title), regex(
      "cyber|hack|breach|vulnerab|security|satellite|iss|artemis|spacecraft|ground.?station|telemetry|command.?link|orion|jwst|hubble|launch|mission|astronaut|rocket|exploration|mars|moon|earth|climate|weather|noaa|weather.?satellite|goes|jpss|spacex|boeing|lockheed|northrop|blue.?origin|ULA|atlas|falcon|starship|crew.?dragon|cargo|resupply|docking|spacewalk|EVA|IVA|esa|european space|space debris|asteroid|planetary defense|space weather|solar|geomagnetic|radiation|microgravity|life support|habitat|gateway|lunar|mars|rover|probe|orbiter|lander|telescope|observatory|deep space|interplanetary|cosmic|universe|galaxy|star|planet|comet|meteor|eclipse|orbit|trajectory|propulsion|fuel|engine|thruster|heat shield|parachute|landing|takeoff|ascent|descent|reentry|ablation|thermal|structural|mechanical|electrical|software|hardware|firmware|avionics|navigation|guidance|control|communication|antenna|transponder|receiver|transmitter|signal|frequency|bandwidth|modulation|encoding|decoding|encryption|decryption|authentication|authorization|integrity|confidentiality|availability|resilience|redundancy|fault tolerance|error detection|error correction|diagnostic|monitoring|telemetry|tracking|command|data|processing|storage|memory|processor|computer|algorithm|machine learning|artificial intelligence|autonomous|robotic|human factors|ergonomics|safety|risk|reliability|quality|assurance|testing|validation|verification|certification|standard|regulation|policy|procedure|protocol|guideline|best practice|lesson learned|anomaly|failure|malfunction|incident|accident|near miss|hazard|threat|vulnerability|mitigation|contingency|emergency|crisis|response|recovery|restoration|resilience|sustainability|environmental|impact|assessment|analysis|research|development|innovation|technology|engineering|science|discovery|exploration|utilization|commercialization|partnership|collaboration|international|cooperation|agreement|treaty|law|ethics|responsibility|accountability|transparency|openness|access|benefit|sharing|equity|justice|fairness|diversity|inclusion|participation|engagement|education|outreach|communication|dissemination|publication|presentation|conference|workshop|seminar|webinar|training|capacity building|knowledge transfer|technology transfer|spin-off|spin-in|application|service|product|market|industry|business|economy|society|culture|history|future|vision|mission|goal|objective|target|milestone|achievement|success|challenge|opportunity|threat|strength|weakness|SWOT|PESTLE|STEEP|scenario|forecast|projection|trend|pattern|insight|finding|conclusion|recommendation|action|plan|strategy|tactic|operation|execution|implementation|deployment|integration|interoperability|compatibility|standardization|harmonization|alignment|coordination|synchronization|optimization|efficiency|effectiveness|performance|productivity|quality|excellence|innovation|creativity|imagination|inspiration|motivation|passion|dedication|commitment|perseverance|resilience|adaptability|flexibility|agility|responsiveness|proactiveness|initiative|leadership|teamwork|collaboration|communication|trust|respect|integrity|honesty|transparency|accountability|responsibility|ethics|values|principles|beliefs|culture|identity|purpose|meaning|fulfillment|satisfaction|happiness|well-being|health|safety|security|protection|defense|resilience|sustainability|future",
      ignore_case = TRUE
    ))) |>
    distinct(url, .keep_all = TRUE)
  
  out_path <- file.path(RAW_DIR, "nasa", sprintf("nasa_raw_%s.rds", SNAPSHOT_TS))
  write_rds(all_items, out_path)
  log_step("NASA", "success", sprintf("Сохранено %d записей", nrow(all_items)))
  invisible(all_items)
}

# ----------------------------------------------------------------------------
# 5. Метаданные
# ----------------------------------------------------------------------------

save_snapshot_meta <- function() {
  meta <- list(
    snapshot_date = SNAPSHOT_DATE,
    snapshot_ts = SNAPSHOT_TS,
    r_version = R.version.string,
    packages = c(
      tidyverse = as.character(packageVersion("tidyverse")),
      httr2 = as.character(packageVersion("httr2"))
    ),
    patterns = SPACE_CYBER_PATTERNS,
    rss_feeds = RSS_FEEDS$feed_url
  )
  out_path <- file.path(RAW_DIR, "meta", sprintf("snapshot_meta_%s.json", SNAPSHOT_TS))
  write_json(meta, out_path, pretty = TRUE, auto_unbox = TRUE)
  log_step("META", "success", out_path)
  invisible(meta)
}

# ----------------------------------------------------------------------------
# 6. Главный пайплайн
# ----------------------------------------------------------------------------

main <- function() {
  log_step("PIPELINE", "start", sprintf("Расширенный срез: %s (диапазон 3 года)", SNAPSHOT_DATE))
  
  rss_data   <- collect_rss(RSS_FEEDS, SNAPSHOT_DATE)
  gdelt_data <- collect_gdelt(SPACE_CYBER_PATTERNS, SNAPSHOT_DATE)
  cisa_data  <- collect_cisa(SNAPSHOT_DATE)
  nasa_data  <- collect_nasa(SNAPSHOT_DATE)
  meta       <- save_snapshot_meta()
  
  summary_tbl <- tibble(
    source = c("RSS", "GDELT", "CISA", "NASA"),
    records = c(nrow(rss_data), nrow(gdelt_data), nrow(cisa_data), nrow(nasa_data))
  ) |>
    mutate(share = sprintf("%.1f%%", 100 * records / sum(records)))
  
  cat("\n")
  cli::cli_h1("Сводка сбора данных")
  print(summary_tbl)
  cat(sprintf("\nВсего сырых упоминаний: %d\n", sum(summary_tbl$records)))
  cli::cli_alert_success("Сырые данные в: %s", RAW_DIR)
  
  invisible(list(rss = rss_data, gdelt = gdelt_data, cisa = cisa_data,
                 nasa = nasa_data, meta = meta))
}

# ВАЖНО: вызываем main(), а НЕ main_clean()!
results <- main()