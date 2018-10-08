window.xhr = (function () {
    this.Request = function Request(method, url, accept, headers) {
        this.method  = method.toLowerCase();
        this.url     = url;
        this.xhr     = new XMLHttpRequest();
        this.headers = {
            'X-Requested-With': 'XMLHttpRequest',
            'Accept': '*/*',
        };
        if (accept) {
            this.headers['Accept'] = accept;
        }
        if (headers) {
            for (var k in headers) {
                this.headers[k] = headers[k];
            }
        }
    };
    this.Request.prototype.send = function (data, callback) {
        var that = this;

        if (data && (Object.getPrototypeOf(data) === Object.prototype)) {
            this.headers['Content-Type'] = 'application/json';
            data = JSON.stringify(data);
        }
        this.xhr.open(this.method, this.url);

        for (k in this.headers) {
            this.xhr.setRequestHeader(k, this.headers[k]);
        }
        this.xhr.onerror = function () {
                callback({ xhr: this });
        };
        this.xhr.onload = function () {
            var contentType = this.getResponseHeader('Content-Type');
            var body        = null;

            if (that.headers['Accept'] != '*/*' && !contentType.startsWith(that.headers['Accept'])) {
                throw new Error("Unexpected Content-Type: " + contentType);
            }

            if (contentType.startsWith('application/json')) {
                body = this.responseText ? JSON.parse(this.responseText) : {};
            } else {
                body = this.responseText;
            }

            if (this.status >= 200 && this.status < 300) { // Success
                callback(null, body);
            } else { // Error
                callback({ status: this.status, body: body, xhr: this });
            }
        };
        this.xhr.send(data);
        return this;
    };
    this.Request.prototype.abort = function () {
        return this.xhr.abort();
    };

    this._request = function (method, url, data, accept, callback) {
        return new this.Request(method, url, accept, {}).send(data, callback);
    };
    this.get = function (url, params, accept, callback) {
        var query = [];
        if (params) {
            for (var k in params) {
                query.push(k + '=' + encodeURIComponent(params[k]));
            }
            url += '?' + query.join('&');
        }
        return this._request('GET', url, null, accept, callback);
    };
    this.getJSON = function (url, params, callback) {
        return this.get(url, params, 'application/json', callback);
    };
    this.getHTML = function (url, params, callback) {
        return this.get(url, params, 'text/html', callback);
    };
    this.post = function (url, params, callback) {
        return this._request('POST', url, params, 'application/json', callback);
    };
    this.del = function (url, params, callback) {
        return this._request('DELETE', url, params, 'application/json', callback);
    };
    this.put = function (url, params, callback) {
        return this._request('PUT', url, params, 'application/json', callback);
    };

    return this;
}).call({});
