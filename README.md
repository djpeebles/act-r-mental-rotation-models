# act-r-mental-rotation-models

Two ACT-R models of mental rotation strategies: "holistic" and "piecemeal".

These models work with ACT-R 7.12.10. To run ACT-R from sources you will also need to have Quicklisp (https://www.quicklisp.org/beta/) installed for your Lisp application.

Installation.

1. Extract the actr7.x.zip file containing the ACT-R code.
2. In the resulting actr7.x folder, create a folder called 'models' and inside that folder create another folder called 'rotation-models'.
3. Place all of the code files into the 'rotation-models' folder.

Running the models

1. In your lisp, load ACT-R the 'load-act-r.lisp' file.
2. Load either the 'rotation-model-holistic.lisp' or 'rotation-model-piecemeal.lisp'
3. Run the model using the function (runsim 30) which will run it for 30 simulated participants.
4. The output you get will hopefully be like the two screenshots, showing the mean RTs for the model for each of the conditions in the experiment together with the correlation and mean deviation between the model and human data.
