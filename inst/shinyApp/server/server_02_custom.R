# Global Functions -----------------------------------------------------

# Standardize the two midpoints
observeEvent(input$midpoint2, {
    updateNumericInput(session,
                       inputId = "midpoint",
                       value = input$midpoint2
    )
})

observeEvent(input$midpoint, {
    updateNumericInput(session,
                       inputId = "midpoint2",
                       value = input$midpoint
    )
})
