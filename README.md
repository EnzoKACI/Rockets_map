# Rockets_map
Mapping previous and upcoming launches based on the Data parsed from the API built by The Space Devs

Built this small dashboard to discover the R programming language and Shiny framework.
Main tasks were: parsing the API, processing the data into tidy formats, building the visualisations (plots and maps)
then implementing these in a reactive Shiny app. Had memory issues with updating the "upcoming launches map" because InvalidateLater 
method needs to be coupled with a "garbage collector" system so we don't "clear markers" every seconds to update the countdowns (thus causing flickers).

First page is a map of all the previous launches in history reported in the dataset built thanks to the
API from www.thespacedevs.com . Panel on the side lets the user change the year and explore the
launches for each selection plus it displays statistics about the success rate and the number of launches
for each launch location (we do insist that a launch location does not always relate to the country’s
initiative and is not always located in a country).

The second page is a live map of all the upcoming launches (from the same database), displaying a
countdown for each launch (minus values being passed launches not yet added to the « previous »
database).

The app updates its launches database by pulling the API on each start.
Documentation says that using the source() method to call the script that updates the database doesn’t
work when the app is deployed to shinyapps.io through rsconnect though it is a wanted behavior in
our case since we don’t want to reach the API limit because too many users would launch the app
through the deployed service. Thus, we let the user create its own instance of the app and call the
update script from there, reaching his own limit with the API.
