select 'select close_session(''' || session_id || ''');' 
, * 
from sessions
where transaction_id <>-1
;


select close_session('ec2-rpt-e1b-dat-rdb-16757:0x3957dc');

select * from query_profiles
where is_executing=true
order by transaction_id desc limit 100;

select close_session('ec2-rpt-e1b-dat-rdb-92677:0xa5345');

select close_session('ec2-rpt-e1b-dat-rdb-7341:0x40367');

--jason