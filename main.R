# MusicPulse - Personal Music Listening & Playlist Analysis System
# Main Menu: connects all modules

# ---- Load all modules ----
source("modules/helpers.R")
source("modules/module1_extract.R")
source("modules/module2_sequence.R")
source("modules/module3_combine.R")
source("modules/module4_monthly.R")
source("modules/module5_average.R")

# ---- Option 6: Music summary ----
view_summary <- function() {
  
  music <- load_music()
  if (is.null(music)) return(invisible(NULL))
  cat("\n--- Music Summary ---\n")
  cat("Total songs:", length(music$Song_ID), "\n")
  cat("Total listening count:", sum(music$Listening_Count), "\n")
  cat("Average rating:", round(mean(music$Rating), 2), "\n")
  cat("Most played song:", music$Song_Name[which.max(music$Listening_Count)],
      "(", max(music$Listening_Count), "plays )\n")
  cat("Least played song:", music$Song_Name[which.min(music$Listening_Count)],
      "(", min(music$Listening_Count), "plays )\n")
  cat("Artists:", paste(unique(music$Artist), collapse = ", "), "\n")
  cat("Genres:", paste(unique(music$Genre), collapse = ", "), "\n")
  cat("Playlists:", paste(unique(music$Playlist), collapse = ", "), "\n")
}

# ---- Show the menu ----
show_menu <- function() {
  cat("\n========================================\n")
  cat("   MusicPulse - Music Listening Analysis\n")
  cat("========================================\n")
  cat("1. Extract Music Data Vector\n")
  cat("2. Generate Track Sequence\n")
  cat("3. Combine Music Vectors\n")
  cat("4. Analyze Monthly Listening\n")
  cat("5. Calculate Average Rating\n")
  cat("6. View Music Summary\n")
  cat("7. Exit\n")
  cat("----------------------------------------\n")
}

# ---- Main program loop ----
repeat {
  show_menu()
  choice <- trimws(readline("Enter your choice (1-7): "))
  
  if (choice == "7") {
    if (!(choice %in% as.character(1:7))) {
      cat("Invalid choice. Please enter a number from 1 to 7.\n")
      next
    }
    cat("\nThank you for using MusicPulse. Goodbye!\n")
    break
  }
  
  switch(choice,
         "1" = extract_vector(),
         "2" = generate_sequence(),
         "3" = combine_vectors(),
         "4" = analyze_monthly(),
         "5" = average_rating(),
         "6" = view_summary(),
         cat("Invalid choice. Please enter a number from 1 to 7.\n"))
  
  readline("\nPress Enter to return to the menu...")
}