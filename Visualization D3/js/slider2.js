/**
 * @file Script con la funcionalidad del slider del mapa
 * @author Pablo de Arriba Mendizábal
 * Realizado a parte para facilitar la actualizacion de la etiqueta del año en timepo de deslizamiento
 */

var output =document.getElementById("actYear2");
var slider =document.getElementById("year2").oninput=function(){
    var value = (this.value-this.min)/(this.max-this.min)*100

    output.innerHTML=this.value;
}