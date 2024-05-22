u__add_text_area <- function(label, shift_function, session) {
  temp_list <- list()
  temp_list$Frame <- RGtk2::gtkFrame()

  RGtk2::gtkFrameSetLabel(temp_list$Frame, label)

  temp_list$View <- RGtk2::gtkTextView()

    RGtk2::gSignalConnect(temp_list$View, "key-release-event", 
                function(view, event, data) {
                  session<- data[[1]]
                  shift_function <- data[[2]]
                    key <- z__event_state_key(event)
                    if(key=="shift+ctrl"){
                    
                    shift_function(session)

                    }
                    return(TRUE)
                },data=list(session,shift_function))

      ############################################
      # Signal whenever code area is edited      #
      # No, I really couldn't find an easier way #
      ############################################
      RGtk2::gSignalConnect(temp_list$View, "backspace", function(view, dummy1) {
        str <- u__text_area_get_text(view)
        #str <- "backspace"
        print(paste0("Detected signal: ", str))
        return(TRUE)
      }, data = temp_list$View)
      #delete-from-cursor needs dummy arguments since delete-from-cursor contains extra info
      RGtk2::gSignalConnect(temp_list$View, "delete-from-cursor", function(str, dummy1, dummy2) {
        str <- "delete"
        print(paste0("Detected signal: ", str))
        return(TRUE)
      })
      RGtk2::gSignalConnect(temp_list$View, "insert-at-cursor", function(str) {
        str <- "insert-at-cursor"
        print(paste0("Detected signal: ", str))
        return(TRUE)
      })
      RGtk2::gSignalConnect(temp_list$View, "paste-clipboard", function(str) {
        str <- "paste-clipboard"
        print(paste0("Detected signal: ", str))
        return(TRUE)
      })
      RGtk2::gSignalConnect(temp_list$View, "cut-clipboard", function(str) {
        str <- "cut-clipboard"
        print(paste0("Detected signal: ", str))
        return(TRUE)
      })
      RGtk2::gSignalConnect(temp_list$View, "preedit-changed", function(str) {
        str <- "preedit-changed"
        print(paste0("Detected signal: ", str))
        return(TRUE)
      })




  temp_list$Scroll <- RGtk2::gtkScrolledWindow()
  RGtk2::gtkScrolledWindowSetPolicy(temp_list$Scroll, "automatic", "automatic")

  RGtk2::gtkContainerAdd(temp_list$Scroll, temp_list$View)
  RGtk2::gtkContainerAdd(temp_list$Frame, temp_list$Scroll)

  RGtk2::gtkWidgetModifyFont(temp_list$View, RGtk2::pangoFontDescriptionFromString("Monospace"))

  temp_list$View["editable"] <- T
  temp_list$View["cursor-visible"] <- T
  temp_list$View["justification"] <- "left" # GtkJustification value
  temp_list$View["left-margin"] <- 10 # 0 is default

  Buffer <- RGtk2::gtkTextViewGetBuffer(temp_list$View)
  RGtk2::gtkTextBufferSetText(Buffer, "")
  return(temp_list)
}


u__text_area_get_text <- function(obj) {
  Buffer <- RGtk2::gtkTextViewGetBuffer(obj$View)
  end_iter <- RGtk2::gtkTextBufferGetEndIter(Buffer)
  start_iter <- RGtk2::gtkTextBufferGetStartIter(Buffer)
  cmd <- RGtk2::gtkTextBufferGetText(Buffer,
    start_iter$iter, end_iter$iter,
    include.hidden.chars = TRUE
  )

  return(cmd)
}



u__text_area_append_text <- function(obj, txt) {
  txt0 <- u__text_area_get_text(obj)
  if (txt0 == "") {
    u__text_area_set_text(obj, txt)
  } else {
    u__text_area_set_text(obj, paste0(txt0, "\n", txt))
  }

  return(T)
}

u__text_area_set_text <- function(obj, txt) {
  Buffer <- RGtk2::gtkTextViewGetBuffer(obj$View)
  RGtk2::gtkTextBufferSetText(Buffer, txt)

  return(T)
}

u__text_area_clear <- function(obj) {
  Buffer <- RGtk2::gtkTextViewGetBuffer(obj$View)
  RGtk2::gtkTextBufferSetText(Buffer, "")

  return(T)
}
