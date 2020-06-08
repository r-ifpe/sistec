# sistec 0.1.0

### Sistec_app

- Included `test_mode` parameter in `sistec_app()`. Now, it is possible to choose 
if you want to run the app in production or in test mode. (#29)

### Refactoring

- Write performace improved in 5x (#34)
- Creation of `rfept_data_frame`. This is useful because we don't need to change code
for every student registration. Now, we just need to create a `read_*()` function that 
converts to a `rfept_data_frame`. 

### Functions 

- The `read_rfept()` is a wrapper around `read_qacademico()` and `read_sigaa()`. Now you just need to specify the folder path and `read_rfept()` identifies if it is a qacademico or sigaa file and then read it. (#38)
- Refactor of `sistec_compare()` to only work with `rfept_data_frame`. (#30)

### Bug fixes
- Fixed bugs when using dplyr 1.2.0. (#28)
