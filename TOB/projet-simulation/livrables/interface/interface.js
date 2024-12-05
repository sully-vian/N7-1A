// Get canvas element and context
var canvas = document.getElementById("particle-canvas");
var ctx = canvas.getContext("2d");

// Generate random colored dots on the canvas
function generateRandomDots(count, color, radius) {
    for (var i = 0; i < count; i++) {
        var x = Math.random() * canvas.width;
        var y = Math.random() * canvas.height;
        ctx.beginPath();
        ctx.arc(x, y, radius, 0, Math.PI * 2);
        ctx.fillStyle = color;
        ctx.fill();
        ctx.closePath();
    }
}

// Populate the graph with random data
function populateGraph() {
    var graph = document.getElementById("graph");
    var data = [];
    for (var i = 0; i < 10; i++) {
        data.push(Math.random() * 100); // Random data points
    }
    var bars = data.map(function (value) {
        return '<div class="bar" style="height:' + value + '%;"></div>';
    });
    graph.innerHTML = bars.join("");
}

ctx.clearRect(0, 0, canvas.width, canvas.height);
// Call functions to generate particles and populate the graph
generateRandomDots(100, "#0000FF", 5); // blue
generateRandomDots(100, "#008000", 5); // green
generateRandomDots(100, "#FF0000", 5); // red

populateGraph();

var ctx2 = document.getElementById("graph").getContext("2d");
var chart = new Chart(ctx2, {
    type: "line",
    data: {
        labels: [
            "0s",
            "10s",
            "20s",
            "30s",
            "40s",
            "50s",
            "1min",
        ],
        datasets: [
            {
                label: "Normalised Total Energy",
                backgroundColor: "rgb(255, 99, 132)",
                borderColor: "rgb(255, 99, 132)",
                data: Array.from({length: 7}, () => Math.random() * 100),
            },
            {
                label: "Density Variance",
                backgroundColor: "rgb(0, 99, 0)",
                borderColor: "rgb(0, 99, 0)",
                data: Array.from({length: 7}, () => Math.random() * 100),
            }
        ],
    },
    options: {},
});
