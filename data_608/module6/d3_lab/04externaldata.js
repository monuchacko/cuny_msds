d3.csv('https://raw.githubusercontent.com/monuchacko/cuny_msds/master/data_608/module6/d3_lab/ue_industry.csv', data => {

    console.log(data);

    const xScale = d3.scaleLinear()
        .domain(d3.extent(data, d => +d.index))
        .range([1180, 20]);
    
    const yScale = d3.scaleLinear()
        .domain(d3.extent(data, d => +d.Agriculture))
        .range([580, 20]);

    d3.select('#part4')
        .selectAll('circle')
        .data(data)
        .enter()
        .append('circle')
        .attr('r', d => 5)
        .attr('cx', d => xScale(d.index))
        .attr('cy', d => yScale(d.Agriculture));

});