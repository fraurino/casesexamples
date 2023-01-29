UPDATE OR INSERT INTO table (col1, col2, col3)
  VALUES ('a', 'b', 'c')
  MATCHING (col1, col2);
  
 


MERGE INTO table AS t
  USING (select 'a' as new1, 'b' as new2, 'c' as new3 from rdb$database) as s
    ON t.col1 = s.new1 and t.col2 = s.new2
  WHEN MATCHED THEN 
    UPDATE SET t.col4 = t.col3
  WHEN NOT MATCHED THEN 
    INSERT (col1, col2, col3) values (s.new1, s.new2, s.new3)
