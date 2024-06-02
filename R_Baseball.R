# R_Baseball.R
# Written by Jang Minjun(202255670)

# Constants
QUIT = 1
MAX_INNINGS = 9
MAX_OUTS = 3
MAX_RUNNERS = 3
MAX_STRIKES = 3
MAX_BALLS = 4

cat("=== Baseball with Computer! ===\n\n")

# Initialize Game
user_team_name = readline("Please input your team name > ")
cat("\n")

user_scores = rep(0, times = 9)
computer_scores = rep(0, times = 9)
user_hits = rep(0, times = 9)
computer_hits = rep(0, times = 9)
user_total = 0
computer_total = 0
quit_flag = 0

# Define game function
play_inning = function(inning, user_turn) {
  cur_runner = 0;
  
  out = 0
  while(out < MAX_OUTS) {
    strike = 0;
    ball = 0;
    while(strike < MAX_STRIKES & ball < MAX_BALLS) {
      if(out != 0 | strike != 0 | ball != 0 | cur_runner != 0)
        cat("------------------------------\n")
      cat(user_team_name, sum(user_scores), ':', sum(computer_scores), "Computer\n")
      cat("Runner :", cur_runner, '\n')
      cat(strike, "Strike", ball, "Ball", out, "Out", "\n\n")
      
      if(user_turn) {
        cat("Computer's throwing a number...\n")
        Sys.sleep(0.5)
        
        user_guess = readline("Please choose your guess (0-9) (Q/q to Quit) > ")
        
        while(!(user_guess %in% as.character(0:9)) & !(user_guess %in% c('Q', 'q'))) {
          cat("\nPlease input between 0-9\n\n")
          user_guess = readline("Please choose your guess (0-9) (Q/q to Quit) > ")
        }
      } else {
        user_guess = readline("Please choose your number to throw (0-9) (Q/q to Quit) > ")
        
        while(!(user_guess %in% as.character(0:9)) & !(user_guess %in% c('Q', 'q'))) {
          cat("\nPlease input between 0-9\n\n")
          user_guess = readline("Please choose your number to throw (0-9) (Q/q to Quit) > ")
        }
      }
      
      if(user_guess %in% c('Q', 'q')) {
        quit_flag <<- QUIT
        
        return()
      }
      
      if(!user_turn) {
        cat("Computer's guessing a number...\n")
        Sys.sleep(0.5)
      }
      
      if(user_turn)
        user_total <<- user_total + 1
      else
        computer_total <<- computer_total + 1
      
      computer_number = sample(0:9, 1)
      
      if(computer_number == as.integer(user_guess)) {
        if(computer_number == 0 | computer_number == 9) {
          cat("\n*** Home Run! *** ( Computer :", computer_number, ")\n\n")
          
          if(user_turn) {
            user_hits[inning] <<- user_hits[inning] + 1
            
            user_scores[inning] <<- user_scores[inning] + cur_runner + 1
          } else {
            computer_hits[inning] <<- computer_hits[inning] + 1
            
            computer_scores[inning] <<- computer_scores[inning] + cur_runner + 1
          }
          
          break;
        } else {
          cat("\n* Hit! * ( Computer :", computer_number, ")\n\n")
          
          if(user_turn)
            user_hits[inning] <<- user_hits[inning] + 1
          else
            computer_hits[inning] <<- computer_hits[inning] + 1
          
          cur_runner = cur_runner + 1
          if(cur_runner > MAX_RUNNERS) {
            if(user_turn)
              user_scores[inning] <<- user_scores[inning] + 1
            else
              computer_scores[inning] <<- computer_scores[inning] + 1
            cur_runner = MAX_RUNNERS
          }
          
          break;
        }
      } else if(computer_number == as.integer(user_guess) + 1 | computer_number == as.integer(user_guess) - 1) {
        cat("\nBall ( Computer :", computer_number, ")\n\n")
        ball = ball + 1
      } else {
        cat("\nStrike! ( Computer :", computer_number, ")\n\n")
        strike = strike + 1
      }
      
      Sys.sleep(1)
    }
    
    if(strike == MAX_STRIKES) {
      cat("Strike Out!\n\n")
      
      out = out + 1
    } else if(ball == MAX_BALLS) {
      cat("Base on Balls\n\n")
      
      cur_runner = cur_runner + 1
      if(cur_runner > MAX_RUNNERS) {
        if(user_turn)
          user_scores[inning] <<- user_scores[inning] + 1
        else
          computer_scores[inning] <<- computer_scores[inning] + 1
        cur_runner = MAX_RUNNERS
      }
    }
    
    Sys.sleep(1)
  }
}

# Start game
cat("==============================\n")
cat("Game start\n")
cat("==============================\n")

Sys.sleep(1)

for(inning in 1:MAX_INNINGS) {
  cat("Inning", inning, '\n')
  cat(user_team_name, "turn!\n")
  cat("==============================\n\n")
  
  play_inning(inning, TRUE)
  
  if(quit_flag == QUIT)
    break
  
  cat("==============================\n")
  cat("Change turn\n")
  cat("==============================\n")
  
  Sys.sleep(1)
  
  cat("Inning", inning, '\n')
  cat("Computer turn!\n")
  cat("==============================\n\n")
  
  cur_runner = 0;
  
  play_inning(inning, FALSE)
  
  if(quit_flag == QUIT)
    break
  
  if(inning != MAX_INNINGS) {
    cat("==============================\n")
    cat("Change turn\n")
    cat("==============================\n")
    
    Sys.sleep(1)
  }
  
  inning = inning + 1
}

cat("\n==============================\n")
cat("Game set!\n")
cat("==============================\n\n")

Sys.sleep(2)

# Show results
cat(1:9, "total  Team\n")
cat(user_scores, sum(user_scores), "\t", user_team_name, "\n")
cat(computer_scores, sum(computer_scores), "\t Computer", "\n\n")

if(sum(user_scores) > sum(computer_scores)) {
  cat("***", user_team_name, "win! ***\n\n")
} else if(sum(user_scores) < sum(computer_scores)) {
  cat("Computer win...\n\n")
} else
  cat("Draw\n\n")

cat("==============================\n\n")

Sys.sleep(1)

# Show batting average and bar plot of hits
if(user_total != 0)
  cat(user_team_name, "batting average :", sum(user_hits) / user_total, '\n')
if(computer_total != 0)
  cat("Computer batting average :", sum(computer_hits) / computer_total, '\n')

plot = barplot(user_hits,
        main = paste(user_team_name, "Hits"),
        ylab = "Hits",
        names.arg = paste0(as.character(1:9), '회'))

text(x = plot, y = user_hits, label = user_hits, pos = 3)
