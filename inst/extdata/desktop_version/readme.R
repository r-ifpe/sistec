# This is just a help to build the desktop version automagic :)
#
#
# 1) build the sistec package (in Rstudio you use ctrl + shift + b)
# 2) create a folder 
# 2) use sistec:::aria_desktop_build("your_folder") 
# 4) install innosetup (if you don't have it installed yet)
# 5) double click the .iss file and run it.

# use this if want to build the entire process automatic
aria_folder <- "C://Users/dmmad/Documents/ARIA/"
aria_desktop_build(aria_folder) 

# use the commands below if you want to run just a part of the process
aria_download_desktop_dependencies(aria_folder) 
aria_desktop_create_iss(aria_folder)
aria_desktop_copy_package(aria_folder)
