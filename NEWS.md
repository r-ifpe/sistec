# sistec 0.2.1

### ARIA

- Fix bug in download button for windows. (#90)

### Functions

- Fixed a bug in `read_sistec()` encode for linux. (#105)
- Implemented `read_suap()`. (#99)
- Implemented `read_conecta()`. (#97)
- Implemented `read_generic_rfept()` and it was wrapped into `read_rfept()`. Now, an institute that was not fully integrated to ARIA can use the app converting your databate to a generic layout. (#92)

### Tests

- CI tests for `write_output()`. (#104)
- CI tests for `aria_server()`. (#103)

### Refactoring

- `write_output()` now writes only non blanked tables. (#104)
- Split ARIA code in `aria_ui()` and `aria_server()`. (#89)

### Security

- Improved security on download button when ARIA is running in a server. (#89)

### Logs

- Created logs. (#89)

# sistec 0.2.0

### ARIA

- Buttons "Comparar" and Downloads appear only when the data uploads. (#83)
- Included entry not found in outputs. (#82)
- Included in outputs CPF's and academic registration with repetitions. (#65)

### Documentation

- ARIA's manual panel. (#86)
- ARIA's manual in pt-br. (#60)

### Functions

- The `aria()` now has offline and online version (#75)
- Creation of `aria_desktop_build()`. This function create a folder with all necessary
files to build the ARIA's desktop version with innosetup. (#74)
- Cration of `read_linked_courses()` to read a file with the relation between courses in academic registration and Sistec cyclo. (#55)
- Update `read_sigaa()`to read datasets with "Campus" and "Cota". (#53)

### Bug fixes

- Fix a bug in sistec utf dates. (#79)
- Fix encoding issues in `read_sistec()`. (#68)

# sistec 0.1.0

### ARIA

- sistec_app was deprecated and point to `aria()`. (#45)
- Included which year to start the comparison. (#44)
- Included wrong registration data frames in the results. (#43)
- Included `test_mode` parameter in `aria()`. Now, it is possible to choose 
if you want to run the app in production or test mode. (#29)
- The web app in this package now has a name: ARIA.

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

- Fixed bugs when using dplyr(>=1.2.0). (#28)
