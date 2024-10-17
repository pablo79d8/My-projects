/**
 * @file Script con las funciones específicas del line chart
 * @author Pablo de Arriba Mendizábal
 */

const COLORES = {"Menores de 25 annos": "blue", "25 y mas annos": "red"};

/**
 * Selecciona los datos de unas edades en concreto
 * @param {JSON} data Datos de los que se quiere obtener el subconjunto
 * @param {String[]} edades edades a seleccionar de los datos
 */
function seleccionarDatosEdades(data, edades) {
    var dataSelec = [];
    data.forEach(function (d) {
        if (edades.includes(d.Edad)) {
            dataSelec.push(d);
        }
    })
    console.log("Datos edades:",dataSelec)
    return dataSelec;
}



/**
 * Selecciona los datos de unas fechas en concreto. 
 * @param {JSON} data Datos de los que se quiere obtener el subconjunto. 
 * @param {String[]} fechas Fechas para las que seleccionar los datos. 
 * @returns 
 */
function filtrarFecha(data, fechas){
    var dataSelec = [];
    
    data.forEach(function(d){
        if(fechas.indexOf(d.Periodo)>-1){
            dataSelec.push(d);
        }
        
    })
    return dataSelec;
}

/**
 * Animaciones de las lineas del line chart
 * @param {*} path 
 */
function transition(path) {
    path.transition()
        .duration(4000)
        .attrTween("stroke-dasharray", tweenDash)
        .on("end", () => { d3.select(this).call(transition); });
}

function tweenDash() {
    if (this instanceof Window)
        return;
    const l = this.getTotalLength(),
        i = d3.interpolateString("0," + l, l + "," + l);
    return function (t) { return i(t) };
}

/**
 * Dibuja el gráfico de la pestaña 1, correspondiente al line chart
 * @param {String[]} edades permite seleccionar el grupo de edad que se quiere representar
 * @param {int} tipoSexo permite seleccionar entre 'Hombres' y 'Mujeres'
 * @param {String[]} fechas intervalo de fechas que se muestran
 */
function dibujarGrafico1(edades, tipoSexo, fechas) {
    d3.csv("./datasets/datosLineChart.csv").then(function (data) {
        console.log("Datos leídos correctamente:", data);
        var margin = { top: 20, right: 10, bottom: 70, left: 150 },
            width = window.innerWidth - 150 - margin.left - margin.right,
            height = 400 - margin.top - margin.bottom;
        
        data = filtrarFecha(data, fechas);

        console.log("Filtrar:", data);
        
        var parseMes = d3.timeParse("%YT%q");
        var tituloY = "Sexos";
        // Tratamiento de datos
        data.forEach(function (d) {
            d.Periodo = parseMes(d.Periodo);
            if (tipoSexo == 0)
                d.Sexo = +d.Hombres;
            else 
                d.Sexo = +d.Mujeres;
        })

        var dataSel = seleccionarDatosEdades(data, edades);

        console.log("Edades:",edades)
        console.log("Datos edades:",dataSel)
        
        // Añadimos el svg al HTML
        var svg = d3.select("#serie")
            .append("svg")
            .attr("width", "100%")
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("width", "100%")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        // Agrupamos los datos por edad para que cada linea del line chart represente una grupo de edad
        var agrupados = d3.nest()
            .key(function (d) { return d.Edad; })
            .entries(dataSel);

        console.log("Datos agrupados:",agrupados)

        // Añadimos el eje X
        var x = d3.scaleTime()
            .domain(d3.extent(data, function (d) { return d.Periodo }))
            .rangeRound([0, width]);
        svg.append("g")
            .attr("transform", "translate(0," + height + ")")
            .call(d3.axisBottom(x).tickFormat(d3.timeFormat("%YT%q")).ticks(fechas.length))
            .selectAll("text")
            .style("text-anchor", "end")
            .attr("dx", "-.8em")
            .attr("dy", ".15em")
            .attr("transform", "rotate(-65)")
            .attr("width", "100%");

        // Añadimos el eje Y
        var y = d3.scaleLinear()
            .domain([0, 70])
            .range([height, 0]);
        
        var y_axis = d3.axisLeft(y).ticks(6);
        svg.append("g")
            .call(y_axis);

        // Dibujamos las líneas
        svg.selectAll(".line")
            .data(agrupados)
            .enter()
            .append("path")
            .call(transition) // animación
            .attr("fill", "none")
            .attr("stroke", function (d) { return COLORES[d.key] }) // Elegimos los mismos colores
            .attr("stroke-width", 1.5)
            .attr("d", function (d) {
                return d3.line()
                    .x(function (d) { return x(d.Periodo); })
                    .y(function (d) { return y(d.Sexo); })
                    (d.values)
            });

        // Añadimos el grid del eje X
        svg.append("g")
            .attr("class", "grid")
            .attr("transform", "translate(0," + height + ")")
            .call(d3.axisBottom(x).ticks(39) // queremos todas las marcas
                .tickSize(-height)
                .tickFormat("")
            )
            .attr("color", "lightGrey");

        // Añadimos el grid del eje Y
        svg.append("g")
            .attr("class", "grid")
            .call(y_axis
                .tickSize(-width)
                .tickFormat("")
            );
        
        // Título del eje Y
        // Cambia en función del sexo seleccionado
        if (tipoSexo == 0)
            tituloY = "Tasa de empleo en Hombres";
        else
            tituloY = "Tasa de empleo en Mujeres";
        
        svg.append("text")
            .attr("transform", "rotate(-90)")
            .attr("y", 50 - margin.left)
            .attr("x", 0 - (height / 2))
            .attr("dy", "1em")
            .style("text-anchor", "middle")
            .text(tituloY);

        // Etiquetas de los puntos del line chart. Solo serán visibles cuando se active el evento mouseover
        // Muestran los datos de empleo
        var tooltip = d3.select('#serie').append('div')
                    .style("position", "absolute")
                    .style("visibility", "hidden")
                    .style("padding", "8px")
                    .style("background-color", "#bc3a3a")
                    .style("opacity", "1")
                    .style("stroke", "black")
                    .style("border-style", "solid")
                    .style("border-width", "1px")
                    .style("text-align", "center")
                    .style("color", "white")
                    .style("font-size", "small")
                    .text("");
        
        var edad = svg.selectAll(".edad")
            .data(agrupados)
            .enter().append("g")
            .attr("class", "edad");

        // Añadimos los puntos
        edad.selectAll("circle")
            .data(function (d) { return d.values })
            .enter()
            .append("circle")
            .attr("r", 5)
            .attr("cx", function (d) { return x(d.Periodo); })
            .attr("cy", function (d) { return y(d.Sexo); })
            .style("fill", function (d) { return COLORES[d.Edad]; })
            .on("mouseover", function (event) {
                d3.select(this).transition()
                    .duration('100')
                    .attr("r", 8); // el punto se hace mas grande cuando el ratón está encima
                tooltip.transition()
                    .duration(100)
                    
                tooltip.html((this.__data__.Sexo) + "% de empleo");
                tooltip.style("visibility", "visible")
                    .style("left", (event.pageX + 10)  + "px")
                    .style("top", (event.pageY + 10) + "px" )
                    .style("background", COLORES[this.__data__.Edad]);

            })
            // Cuando se retira el cursor, se oculta la etiqueta y el punto vuelve al tamaño original
            .on("mouseout", function () {
                d3.select(this).transition()
                    .duration('100')
                    .attr("r", 5);
                // desaparecer etiquetas
                tooltip.transition()
                    .duration(100)
                    .style("visibility", "hidden");
            });
    })
}