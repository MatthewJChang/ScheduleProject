data = read.csv("data.csv", stringsAsFactors = FALSE)
classes = data
for (i in 1:ncol(classes)) {
  classes[, i] = toupper(classes[, i])
}
colnames(classes) = c("Row Number", "Control Number", "Course Number", "Section", "Day Hour", "Room", "Course Title", "Unit Credit", "Instructor", "Exam Group", "Restrictions", "Department")

readBuilding = function() {
  building = toupper(readline(prompt = "Enter Building: "))
  wantRoom = "no"
  wantRoom = tolower(readline(prompt = "Do you want a specific room? "))
  if (wantRoom == "no") {
    return(building)
  } 
  room = toupper(readline(prompt = "Enter Room Number: "))
  return(c(building, room))
}

getClasses =  function() {
  location = readBuilding()
  if (length(location) == 1) {
    index = which(grepl(location, classes[, "Room"]))
    matches = classes[index, ]
  } else {
    index = which(grepl(location[1], classes[, "Room"]) & grepl(location[2], classes[, "Room"]))
    matches = classes[index, ]
  }
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

readAll = function() {
  control = toupper(readline(prompt = "Enter Control Number:  "))
  department = toupper(readline(prompt = "Enter Department: "))
  course = toupper(readline(prompt = "Enter Course Number:  "))
  section = toupper(readline(prompt = "Enter Section: "))
  day = toupper(readline(prompt = "Enter Day: "))
  time = toupper(readline(prompt = "Enter Time: "))
  building = toupper(readline(prompt = "Enter Building: "))
  room = toupper(readline(prompt = "Enter Room Number: "))
  title = toupper(readline(prompt = "Enter Title Keywords: "))
  credit = toupper(readline(prompt = "Enter Credit Units: "))
  instructor = toupper(readline(prompt = "Enter Instructor: "))
  group = toupper(readline(prompt = "Enter Exam Group: "))
  return(c(control, department, course, section, day, time, building, room, title, credit,
           instructor, group))
}

listClasses = function() {
  location = readAll()
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
