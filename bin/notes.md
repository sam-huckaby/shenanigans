# How TUIs work

A TUI is a model-view-controller paradigm, wherein you have:

- a **model** which is the state of the program, and every stored value
- a **view** which is the application's visible state given the current model
- a **controller** (maybe more than one?) that allows a user to modify the model and thereby modify the view

TUIs operate within something that is essentially a game loop. Each time a controller is utilized, the loop runs another iteration.

A common lifecycle looks like this:

- Program starts
- Model is initialized with default data or is populated from a data source
- Model is passed to a render function, or render is just called and it gets the model
- The render function draws the view on the screen according to the model

At this point the game loop takes over

- The app begins accepting input from the user
- When the user provides input, the input is given to a controller or a handler that decides on the right controller
- The controller attempts to modify the model in some way
- The render function draws the view on the screen according to the updated model

