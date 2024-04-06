#' e__move_column
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param df_obj TODO
#' @param outer_env TODO
#'
#' @return TODO

e__move_column <- function(placement, session_name, current_row, outer_env=totem) {
  require(RGtk2)
  
  #Get selection
  selection <- as.character(current_row$column)
  #Get column order of dataset
  st <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$select_entry)
  temp_df <- outer_env[[session_name]]$data2
  if (st != "") {
    #Convert st into a vector of strings
    vst <- unlist(strsplit(gsub(" ", "", st), ","))
    dataset2 <- select(temp_df[0, ], vst)
    col_order <- colnames(dataset2)
  } else {
    col_order <- colnames(temp_df[0, ])
  }

  #Handle moving column before target
  if (placement == 0) {
    #Generate dialog to ask about placement
    dialog <- gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window, 
      flags = "destroy-with-parent", 
      type = "question", 
      buttons = "ok-cancel", 
      paste0("Select a column to move ", toupper(selection), " before"))
    
    #Add options
    choices <- col_order
    combo <- gtkComboBoxNewText()
    combo$show()
    for (choice in choices) {
      combo$appendText(choice)
    }
    combo$setActive(0)
    
    #Make a frame for the options
    frame <- gtkFrame(paste0("Column to move ", toupper(selection), " before"))
    frame$add(combo)
    dialog[["vbox"]]$add(frame)
    #Require response before interacting with table
    response <- dialog$run()  
    #Find selection
    target <- col_order[gtkComboBoxGetActive(combo) + 1]
    #Destroy dialog box
    gtkWidgetDestroy(dialog)
    
    if (response %in% c(GtkResponseType["close"], GtkResponseType["delete-event"], GtkResponseType["cancel"]) == F) {
      old_index <- which(col_order == selection)
      new_index <- which(col_order == target) + placement
      if (new_index > old_index) {
        delete_index <- old_index
      } else {
        delete_index <- old_index + 1
      }
      col_order <- append(col_order, selection, after = new_index - 1)
      col_order <- col_order[-delete_index]
      #Turn col_order into a comma separated list
      newst <- ""
      for (i in 1:length(col_order)) {
        newst <- paste0(newst, col_order[i])
        if (i != length(col_order)) {
          newst <- paste0(newst, ", ")
        }
      }
      RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$select_entry, newst)
    }
  }
}
