window.LIT = window.LIT || {};
LIT.Reader = LIT.Reader || {};
LIT.Reader.CacheEvents = {
    local: window.applicationCache,
    statusValues: ['uncached', 'idle', 'checking', 'downloading', 'updateready', 'obsolete'],
    events: ['cached', 'checking', 'downloading', 'error', 'noupdate', 'obsolete', 'progress', 'updateready'],
    logCacheEvent: function (e) {
        var online, status, type, message;
        online = (navigator.onLine) ? 'yes' : 'no';
        status = LIT.Reader.CacheEvents.statusValues[LIT.Reader.CacheEvents.local.status];
        type = e.type;
        message = 'nav online: ' + online;
        message += ', reader online: ' + (window.READER_OFFLINE ? 'no': 'yes');
        message += ', event: ' + type;
        message += ', status: ' + status;
        if (type == 'error' && navigator.onLine) {
            if(window.READER_OFFLINE){
                message += ' (offline, so manifest cannot be fetched)';
            }else{
                message += ' (probably a syntax error in manifest)';
            }
        }
        console.log(message);
        if (type == "downloading") {
            console.log("Downloading Resources..");
        }
        if (type == 'updateready') {
            LIT.Reader.CacheEvents.local.swapCache();
            console.log('swap cache has been called');
            window.location.reload();
        }
        if (type == "obsolete") {
            console.log("Cache is marked obsolete, refreshing page.");
            window.location.reload();
        }
    },
    updateCache: function () {
        LIT.Reader.CacheEvents.local.update();
    },
    init: function () {
        //only if applicationCache is available
        if (typeof this.local !== "undefined") {
            for (var i = 0; i < this.events.length; i++) {
                this.local.addEventListener(this.events[i], this.logCacheEvent, false);
            }
        }
    }
};
LIT.Reader.CacheEvents.init();
