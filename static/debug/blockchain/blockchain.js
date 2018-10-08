
var interval = setInterval(updateBlockchain, 1000);

function updateBlockchain() {
    xhr.getJSON("/blockchain/best", {depth: 10000, pretty: true}, function (err, resp) {
        if (err !== null || resp.tag != "Ok") {
            clearInterval(interval);
            return;
        }
        var blocks = resp.contents;

        renderBlockchain(blocks);
    });
}


function renderBlockchain(blks) {
    var main = document.querySelector('main');
    main.innerHTML = '';
    blks.forEach(function (blk) {
        var e = renderBlock(blk);
        main.appendChild(e);
    });
}

function renderBlock(blk) {
    var e = document.querySelector('#block-template').cloneNode(true);
    e.style = 'display: block';
    e.querySelector('.block-hash').innerHTML = blk.hash;
    e.querySelector('.block-parent').innerHTML = blk.header.parentHash;
    e.querySelector('.block-timestamp').innerHTML = blk.header.timestamp;
    e.querySelector('.block-nonce').innerHTML = blk.header.nonce;
    e.querySelector('.block-difficulty').innerHTML = blk.header.difficulty;

    blk.data.forEach(function (tx) {
        var txe = document.createElement('pre');
        txe.classList.add('block-data-tx');
        txe.innerHTML = tx.msg;
        e.querySelector('.block-data').appendChild(txe);
    });
    return e;
}
