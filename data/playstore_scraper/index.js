var gplay = require('google-play-scraper');
var jsonfile = require('jsonfile');

var appsFile = __dirname + '/apps.json';
//terms = ["quit smoking", "stop smoking", "smoking cessation", "alcohol", "quit drinking", "quit substance", "smoking drinking depression"];

//terms.forEach(function (item, index) {

//    console.log("searching for" + item + "....");
//    gplay.search({
//        term: item,
//        num: 150,
//        throttle: 10
//    }).then(console.log)
//});

// Note: I also added the search terms "depression", "treatment", "smoking depression"

gplay.search({
    term: "treatment",
    num: 100
}).then(function (lists) {
    // have fun with the app data here
    save(lists);
}, function (data) {
    console.log(data);
});



function save(response) {
    console.log("saving response....");
    apps = jsonfile.readFileSync(appsFile);
    apps = [apps,response]
    jsonfile.writeFile(appsFile, apps, function (err) {
        console.error(err);
    });
}
