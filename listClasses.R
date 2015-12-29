listClasses = function(control, department, course, section, day, time, building, room, 
                       title, credit, instructor, group) {
  data = read.csv("data.csv", stringsAsFactors = FALSE)
  classes = data
  for (i in 1:ncol(classes)) {
    classes[, i] = toupper(classes[, i])
  }
  colnames(classes) = c("Row Number", "Control Number", "Course Number", "Section", "Day Hour",
                        "Room", "Course Title", "Unit Credit", "Instructor", "Exam Group", 
                        "Restrictions", "Department")
  location = c(control, department, course, section, day, time, building, room, 
               title, credit, instructor, group)
  
  index = which(grepl(location[7], classes[, "Room"]) 
                & grepl(location[8], classes[, "Room"])
                & grepl(location[1], classes[, "Control Number"])
                & grepl(location[2], classes[, "Department"])
                & grepl(location[3], classes[, "Course Number"])
                & grepl(location[4], classes[, "Section"])
                & grepl(location[5], classes[, "Day Hour"])
                & grepl(location[6], classes[, "Day Hour"])
                & grepl(location[9], classes[, "Course Title"])
                & grepl(location[10], classes[, "Unit Credit"])
                & grepl(location[11], classes[, "Instructor"])
                & grepl(location[12], classes[, "Exam Group"])
  )            
  matches = classes[index, ]
  return(matches)
  print("Classes matching query: ") 
  for (i in 1: nrow(matches)) {
    print(paste0("Control Number: ", matches[i, "Control Number"]))
    print(paste0("Course Number: ", matches[i, "Course Number"]))
    print(paste0("Section: ", matches[i, "Section"]))
    print(paste0("Day Hour: ", matches[i, "Day Hour"]))
    print(paste0("Room: ", matches[i, "Room"]))
    print(paste0("Course Title: ", matches[i, "Course Title"]))
    print(paste0("Instructor: ", matches[i, "Instructor"]))
    print(paste0("Department: ", matches[i, "Department"]))
    print(" ")
  }
}
