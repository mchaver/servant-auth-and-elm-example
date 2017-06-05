var _mchaver$servant_auth_and_elm_example$Native_CsrfCookie = function() {

  var scheduler = _elm_lang$core$Native_Scheduler;
  var value = {};

  value.csrfCookie = e => {
    return scheduler.nativeBinding(callback => {
      let r = document.cookie.match(new RegExp('XSRF-TOKEN=([^;]+)'));
      if (r) {
        callback(scheduler.succeed(r[1]));
      } else {
        callback(scheduler.fail([]));
      }
    })
  };
  
  value.deleteCookie = e => {
    return scheduler.nativeBinding(callback => {
      console.log('Delete Cookie');
      //function delete_cookie(name) {
      //  document.cookie = name +'=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;';
      //}
      document.cookie = 'XSRF-TOKEN=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;';
      window.location.href = '/';
      callback(scheduler.succeed([]));
    })
  };

  return value;

}();
