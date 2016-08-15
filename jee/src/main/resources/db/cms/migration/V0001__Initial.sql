create table users (
    id serial not null primary key,
    username varchar(32) not null unique,
    email varchar(64),
    password varchar(128) not null,
    nickname varchar(64),
    auth_token varchar(128) not null unique,
    auth_token_expires_at timestamp not null,
    admin_user boolean not null default false,
    created_at timestamp not null default current_timestamp,
    updated_at timestamp not null default current_timestamp
);

create table entries (
    id serial not null primary key,
    user_id bigint unsigned not null,
    prefix varchar(128) not null default '',
    name varchar(128) not null,
    title varchar(128) not null,
    description text,
    page_image varchar(128),
    content text,
    data text,
    format varchar(16) not null default 'markdown',
    visible boolean not null default false,
    published_at datetime,
    created_at timestamp not null default current_timestamp,
    updated_at timestamp not null default current_timestamp,
    unique(prefix,name),
    FOREIGN KEY(user_id) REFERENCES users(id)
);

create index entries_prefix on entries(prefix);

-- ラベルは検索対象となるので jsonの中には入れられない
create table entries_labels (
    entry_id bigint unsigned not null,
    label varchar(32) not null,
    primary key(entry_id,label),
    FOREIGN KEY(entry_id) REFERENCES entries(id)
);

create table prefixes (
    prefix varchar(128) not null primary key,
    user_id bigint unsigned,
    data text,
    FOREIGN KEY(user_id) REFERENCES users(id)
);

create table media (
    prefix varchar(128) not null default '',
    hash varchar(64) not null,
    content_type varchar(64) not null,
    content_length int not null,
    title varchar(128),
    description text,
    user_id bigint unsigned,
    created_at datetime not null default current_timestamp,
    updated_at datetime not null default current_timestamp,
    primary key(prefix,hash),
    FOREIGN KEY(user_id) REFERENCES users(id)
);

create table blobs (
    id varchar(128) not null primary key,
    content LONGBLOB not null
);
