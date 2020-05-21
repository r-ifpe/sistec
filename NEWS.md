# sistec 0.1.0

### Sistec_app

- Included `test_mode` parameter in `sistec_app()`. Now, it is possible to choose 
if you want to run the app in production or in test mode. (#29)

### Refactoring

- Creation of `rfept_data_frame`. This is useful because we don't need to change code
for every student registration. Now, we just need to create a `read_*()` function that 
converts to a `rfept_data_frame`. 

### Functions 
- Refactor of `sistec_compare()` to only work with `rfept_data_frame`. (#30)

### Bug fixes
- Fixed bugs when using dplyr 1.2.0. (#28)
