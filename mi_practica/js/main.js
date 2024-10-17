/**
 * @file Script con las funciones generales para el manejo de los graficos
 * @author Pablo de Arriba Mendizábal
 */


  //Cuando se carga la página
 $(document).ready(function () {
    let edades = ["Menores de 25 annos", "25 y mas annos"];
      var sexo = 0;
    var sexoTab2 = 0;
    var anno = 2014;

    var fechas_selected = [
        "2014T1", "2014T2", "2014T3", "2014T4",
        "2015T1", "2015T2", "2015T3", "2015T4",
        "2016T1", "2016T2", "2016T3", "2016T4",
        "2017T1", "2017T2", "2017T3", "2017T4",
        "2018T1", "2018T2", "2018T3", "2018T4",
        "2019T1", "2019T2", "2019T3", "2019T4",
        "2020T1", "2020T2", "2020T3", "2020T4",
        "2021T1", "2021T2", "2021T3", "2021T4",
        "2022T1", "2022T2", "2022T3", "2022T4",
        "2023T1", "2023T2", "2023T3"
      ];

    // Manejador de tabs
    $('#tabs li a:not(:first)').addClass('inactive');
    $('.container').hide();
    $('.container:first').show();
    $('#tabs li a').click(function () {
        var t = $(this).attr('id');
        if ($(this).hasClass('inactive')) {
            $('#tabs li a').addClass('inactive');
            $(this).removeClass('inactive');

            $('.container').hide();
            $('#' + t + 'C').fadeIn('slow');

            // Cargamos cada tab solo cuando está seleccionado
            switch (t) {
                case "tab1":
                    d3.select("#serie").selectAll("svg").remove();
                    dibujarGrafico1(edades, sexo, fechas_selected);
                    break;
                case "tab2":
                    d3.select("#mapa").selectAll("svg").remove();
                    document.getElementById("radiosTerminos2").value = 1;
                    dibujarGrafico2(1);
                    break;
            }
        }
    });


    // El tab1 se carga al principio
    d3.select("#serie").selectAll("svg").remove();
    dibujarGrafico1(edades, sexo, fechas_selected);

   
    // Gráfico 1
    // Para la selección de las edades de line chart
    $("input[type=checkbox]").change(function () {
        if (this.checked) {
            edades.push(this.name);
        } else {
            edades.splice(edades.indexOf(this.name), 1);
        }
        console.log(edades)
        d3.select("#serie").selectAll("svg").remove();
        dibujarGrafico1(edades, sexo, fechas_selected);
    });


    $("#selectSexo").change(function () {
        sexo = parseInt($(this).val());
        d3.select("#serie").selectAll("svg").remove();
        dibujarGrafico1(edades, sexo, fechas_selected);
    });

    // Para seleccionar las fechas con el slider
    var tooltip = d3.select('#container-slider').append('div')
                .style("position", "absolute")
                .style("visibility", "hidden")
                .style("padding", "30px")
                .style("margin-top", "4px")
                .style("height", "20px")
                .style("background-color", "lightgrey")
                .style("opacity", "1")
                .style("stroke", "black")
                .style("border-style", "solid")
                .style("border-width", "1px")
                .style("text-align", "center")
                .style("color", "black")
                .style("font-size", "small")
                .text("");
    
    var handle = $( "#custom-handle" );
    $( "#slider-range" ).slider({
        orientation: "horizontal",
        range: true,
        min: 0,
        max: 38,
        values: [ 0, 38 ],
        labels: "thtrhw ",

        create: function() {
            handle.text( $( this ).slider( "value" ) );
        },
        slide: function(event, ui) {
            tooltip.transition()
                    .duration(100)
            
            var fechas = [
                "2014T1", "2014T2", "2014T3", "2014T4",
                "2015T1", "2015T2", "2015T3", "2015T4",
                "2016T1", "2016T2", "2016T3", "2016T4",
                "2017T1", "2017T2", "2017T3", "2017T4",
                "2018T1", "2018T2", "2018T3", "2018T4",
                "2019T1", "2019T2", "2019T3", "2019T4",
                "2020T1", "2020T2", "2020T3", "2020T4",
                "2021T1", "2021T2", "2021T3", "2021T4",
                "2022T1", "2022T2", "2022T3", "2022T4",
                "2023T1", "2023T2", "2023T3"
                ];
            tooltip.html(fechas[ui.values[ui.handleIndex]] );
                tooltip.style("visibility", "visible")
               .style("left", event.pageX + "px")
               .style("top", (event.pageY - 50) + "px"); 
        },

        change: function(event, ui ) {

        tooltip.transition()
                .duration(100)
                .style("visibility", "hidden");
     
        var fechas = [
            "2014T1", "2014T2", "2014T3", "2014T4",
            "2015T1", "2015T2", "2015T3", "2015T4",
            "2016T1", "2016T2", "2016T3", "2016T4",
            "2017T1", "2017T2", "2017T3", "2017T4",
            "2018T1", "2018T2", "2018T3", "2018T4",
            "2019T1", "2019T2", "2019T3", "2019T4",
            "2020T1", "2020T2", "2020T3", "2020T4",
            "2021T1", "2021T2", "2021T3", "2021T4",
            "2022T1", "2022T2", "2022T3", "2022T4",
            "2023T1", "2023T2", "2023T3"
            ];
        fechas_selected = fechas.slice(ui.values[0], ui.values[1]+1);
        d3.select('#rangoFechas').text(fechas_selected[0] + " - " + fechas_selected.slice(-1)[0]);
        d3.select("#serie").selectAll("svg").remove();
        dibujarGrafico1(edades, sexo, fechas_selected);

        },
 
    });

     // Evento para el boton de restablecer parámetros
    document.getElementById("refresh-button").addEventListener("click", function() {
        location.reload(); // Esto recarga la página actual
        document.getElementById("selectSexo").selectedIndex = 0;
        document.getElementById("+25").checked = true;
        document.getElementById("-25").checked = true;
    });
    
    

    // Gráfico 2
    // Selección del sexo mostrado
    $("select").on('change', function () {
        d3.select("#mapa").selectAll("svg").remove();
        dibujarGrafico2(this.value);
        });
    $("input[type=range]")
        .change(cambiar2)
    ;
    
});

