d3.csv('ue_industry.csv', data => {

    console.log(data);

    // Define your scales and generator here.
    const xScale = d3.scaleLinear()
        .domain(d3.extent(data, d => +d.index))
        .range([1180, 20]);

    d3.select('#answer1')
        // append more elements here

});
