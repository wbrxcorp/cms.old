CREATE TABLE `mt_author` (
  `author_id` serial NOT NULL primary key,
  `author_name` varchar(255) NOT NULL,
  `author_nickname` varchar(255) DEFAULT NULL,
  `author_password` varchar(60) NOT NULL,
  KEY `mt_author_name` (`author_name`)
);

insert into mt_author(author_name,author_nickname,author_password) values('scott','スコットさん','cgy57J/o2gD8Q') -- tiger
