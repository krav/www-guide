import './main.css';
import guide from './guide.csv';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Elm.Main.init({
    node: document.getElementById('root'),
    flags: [guide, JSON.parse(localStorage.getItem("favs")) || []]
});

app.ports.storeFavs.subscribe(function(data){
    localStorage.setItem('favs', JSON.stringify(data));
});

registerServiceWorker();

// hacky stuff stolen from the internet
function resizeGridItem(item){
    //const grid = document.getElementsByClassName("events")[0];
    const grid = item.parentElement;
    const rowHeight = parseInt(window.getComputedStyle(grid).getPropertyValue('grid-auto-rows'));
    const rowGap = parseInt(window.getComputedStyle(grid).getPropertyValue('grid-row-gap'));
    const rowSpan = Math.ceil((item.querySelector('.content').getBoundingClientRect().height+rowGap)/(rowHeight+rowGap))+1;
    item.style.gridRowEnd = "span "+rowSpan;
}

function resizeAllGridItems(){
    const allItems = document.getElementsByClassName("event");
    var x;
    for(x=0;x<allItems.length;x++){
        resizeGridItem(allItems[x]);
    }
}

window.addEventListener("resize", resizeAllGridItems);
var observer = new MutationObserver(resizeAllGridItems);
observer.observe(document, {subtree: true, childList: true});

