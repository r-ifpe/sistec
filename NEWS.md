# sistec 0.1.0

### ARIA

- The web app in this package now has a name: ARIA.
- Included `test_mode` parameter in `aria()`. Now, it is possible to choose 
if you want to run the app in production or test mode. (#29)
- Included wrong registration data frames in the results. (#43)
- Included which year to start the comparison. (#44)
- sistec_app was deprecated and point to `aria()`. (#45)

### Refactoring

- Write performance improved in 5x (#34)
- Creation of `rfept_data_frame`. This is useful because we don't need to change the code
for every student registration. Now, we just need to create a `read_*()` function that 
converts to a `rfept_data_frame`. (#30)

### Functions 

- The `read_rfept()` is a wrapper around `read_qacademico()` and `read_sigaa()`. Now you just need to specify the folder path and `read_rfept()` identifies if it is a qacademico or sigaa file and then read it. (#38)
- Sigaa integration on package using `read_sigaa()`. (#33)
- Refactor of `sistec_compare()` to only work with `rfept_data_frame`. (#30)

### Bug fixes

- Fixed bugs when using dplyr 1.2.0. (#28)
