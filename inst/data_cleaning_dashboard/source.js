$(document).ready(function(){
  $('input[name=ADJ_SET_algo]').on('click', function(event){
    if($('input[name=ADJ_SET_algo]:checked').length == 0){
      $(this).prop('checked', true);
    }
  });
});