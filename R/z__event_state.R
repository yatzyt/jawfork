z__event_state <- function(event) {
  click_btn <- event$button
  if (click_btn == 1) {
    btn <- "left"
  } else if (click_btn == 2) {
    btn <- "middle"
  } else if (click_btn == 3) {
    btn <- "right"
  } else {
    btn <- "other"
  }

  click_state <- as.numeric(RGtk2::gdkEventGetState(event)[2])

  if (click_state == 0) {
    state <- "none"
  } else if (click_state == 1) {
    state <- "shift"
  } else if (click_state == 2) {
    state <- "none+cap"
  } else if (click_state == 3) {
    state <- "shift+cap"
  } else if (click_state == 4) {
    state <- "ctrl"
  } else if (click_state == 5) {
    state <- "ctrl+shift"
  } else if (click_state == 6) {
    state <- "ctrl+cap"
  } else if (click_state == 7) {
    state <- "ctrl+shift+cap"
  } else if (click_state == 8) {
    state <- "alt"
  } else if (click_state == 9) {
    state <- "alt+shift"
  } else if (click_state == 10) {
    state <- "alt+cap"
  } else if (click_state == 11) {
    state <- "alt+shift+cap"
  } else if (click_state == 12) {
    state <- "ctrl+alt"
  } else if (click_state == 13) {
    state <- "ctrl+alt+shift"
  } else if (click_state == 14) {
    state <- "ctrl+alt+cap"
  } else if (click_state == 14) {
    state <- "ctrl+alt+shift+cap"
  } else {
    state <- "none"
  }


  return(paste0(btn, "+", state))
}



z__event_state_key <- function(event) {
  key <- paste0(
    event[["keyval"]], "+",
    event[["state"]]
  )

  
  if (key %in% c("65506+4","65505+4","65508+1","65507+1")) {
    return("shift+ctrl")
  }

  return("")
}
