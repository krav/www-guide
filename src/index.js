import './main.css';
import guide from './guide.csv';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.Main.init({
    node: document.getElementById('root'),
    flags: guide
});

registerServiceWorker();
