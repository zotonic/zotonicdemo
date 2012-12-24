--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: oauth_token_type; Type: TYPE; Schema: public; Owner: zotonic
--

CREATE TYPE oauth_token_type AS ENUM (
    'request',
    'access'
);


ALTER TYPE public.oauth_token_type OWNER TO zotonic;

--
-- Name: medium_delete(); Type: FUNCTION; Schema: public; Owner: zotonic
--

CREATE FUNCTION medium_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    begin
        if (tg_op = 'DELETE') then
            if (old.filename <> '' and old.filename is not null and old.is_deletable_file) then
                insert into medium_deleted (filename) values (old.filename);
            end if;
            if (old.preview_filename <> '' and old.preview_filename is not null and old.is_deletable_preview) then
                insert into medium_deleted (filename) values (old.preview_filename);
            end if;
        end if;
        return null;
    end;
    $$;


ALTER FUNCTION public.medium_delete() OWNER TO zotonic;

--
-- Name: rsc_pivot_update(); Type: FUNCTION; Schema: public; Owner: zotonic
--

CREATE FUNCTION rsc_pivot_update() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    declare 
        duetime timestamp;
        do_queue boolean;
    begin
        if (tg_op = 'INSERT') then
            do_queue := true;
        elseif (new.version <> old.version or new.modified <> old.modified) then
            do_queue := true;
        else
            do_queue := false;
        end if;

        if (do_queue) then
            <<insert_update_queue>>
            loop
                update rsc_pivot_queue 
                set due = (case when now() < due then now() else due end),
                    serial = serial + 1
                where rsc_id = new.id;
            
                exit insert_update_queue when found;
            
                begin
                    insert into rsc_pivot_queue (rsc_id, due, is_update) values (new.id, now(), tg_op = 'UPDATE');
                    exit insert_update_queue;
                exception
                    when unique_violation then
                        -- do nothing
                end;
            end loop insert_update_queue;
        end if;
            
        if (new.is_protected) then
            begin
                insert into protect (id) values (new.id);
            exception
                when unique_violation then
                    -- do nothing
            end;
        else
            delete from protect where id = new.id;
        end if;
        return null;
    end;
    $$;


ALTER FUNCTION public.rsc_pivot_update() OWNER TO zotonic;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: category; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE category (
    id integer NOT NULL,
    parent_id integer,
    seq integer DEFAULT 1000000 NOT NULL,
    nr integer DEFAULT 0 NOT NULL,
    lvl integer DEFAULT 0 NOT NULL,
    lft integer DEFAULT 0 NOT NULL,
    rght integer DEFAULT 0 NOT NULL,
    props bytea
);


ALTER TABLE public.category OWNER TO zotonic;

--
-- Name: comment; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE comment (
    id integer NOT NULL,
    is_visible boolean DEFAULT true NOT NULL,
    rsc_id integer NOT NULL,
    user_id integer,
    persistent_id character varying(32),
    gravatar_code character varying(40) DEFAULT ''::character varying NOT NULL,
    email character varying(80) DEFAULT ''::character varying NOT NULL,
    name character varying(80) DEFAULT ''::character varying NOT NULL,
    user_agent character varying(250) DEFAULT ''::character varying NOT NULL,
    ip_address character varying(40) DEFAULT ''::character varying NOT NULL,
    keep_informed boolean DEFAULT false NOT NULL,
    props bytea,
    created timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.comment OWNER TO zotonic;

--
-- Name: comment_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_id_seq OWNER TO zotonic;

--
-- Name: comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE comment_id_seq OWNED BY comment.id;


--
-- Name: comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('comment_id_seq', 1, false);


--
-- Name: config; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE config (
    id integer NOT NULL,
    module character varying(80) DEFAULT 'zotonic'::character varying NOT NULL,
    key character varying(80) DEFAULT ''::character varying NOT NULL,
    value character varying(1000) DEFAULT ''::character varying NOT NULL,
    props bytea,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.config OWNER TO zotonic;

--
-- Name: config_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE config_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.config_id_seq OWNER TO zotonic;

--
-- Name: config_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE config_id_seq OWNED BY config.id;


--
-- Name: config_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('config_id_seq', 4, true);


--
-- Name: edge; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE edge (
    id integer NOT NULL,
    subject_id integer NOT NULL,
    predicate_id integer NOT NULL,
    object_id integer NOT NULL,
    seq integer DEFAULT 1000000 NOT NULL,
    creator_id integer,
    created timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.edge OWNER TO zotonic;

--
-- Name: edge_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE edge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.edge_id_seq OWNER TO zotonic;

--
-- Name: edge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE edge_id_seq OWNED BY edge.id;


--
-- Name: edge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('edge_id_seq', 1, false);


--
-- Name: emailq; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE emailq (
    id integer NOT NULL,
    status character varying(10) DEFAULT 'new'::character varying NOT NULL,
    retry_on timestamp with time zone DEFAULT (now() + '00:10:00'::interval),
    retry integer DEFAULT 0 NOT NULL,
    sender character varying(100),
    recipient character varying(100),
    props bytea,
    sent timestamp with time zone,
    created timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.emailq OWNER TO zotonic;

--
-- Name: emailq_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE emailq_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.emailq_id_seq OWNER TO zotonic;

--
-- Name: emailq_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE emailq_id_seq OWNED BY emailq.id;


--
-- Name: emailq_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('emailq_id_seq', 1, false);


--
-- Name: identity; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE identity (
    id integer NOT NULL,
    rsc_id integer NOT NULL,
    type character varying(32) DEFAULT ''::character varying NOT NULL,
    key character varying(200) DEFAULT ''::character varying NOT NULL,
    is_unique boolean,
    is_verified boolean DEFAULT false NOT NULL,
    verify_key character varying(32),
    propb bytea,
    prop1 character varying(200) DEFAULT ''::character varying NOT NULL,
    prop2 character varying(200) DEFAULT ''::character varying NOT NULL,
    prop3 character varying(200) DEFAULT ''::character varying NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone DEFAULT now() NOT NULL,
    visited timestamp with time zone
);


ALTER TABLE public.identity OWNER TO zotonic;

--
-- Name: identity_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE identity_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.identity_id_seq OWNER TO zotonic;

--
-- Name: identity_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE identity_id_seq OWNED BY identity.id;


--
-- Name: identity_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('identity_id_seq', 2, true);


--
-- Name: log; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE log (
    id bigint NOT NULL,
    rsc_id integer,
    user_id integer,
    type character varying(80) DEFAULT ''::character varying NOT NULL,
    module character varying(160) DEFAULT ''::character varying NOT NULL,
    props bytea,
    created timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.log OWNER TO zotonic;

--
-- Name: log_email; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE log_email (
    id bigint NOT NULL,
    severity integer DEFAULT 1 NOT NULL,
    message_nr character varying(32),
    mailer_status character varying(32),
    mailer_message bytea,
    mailer_host character varying(128),
    envelop_to character varying(128) NOT NULL,
    envelop_from character varying(128) NOT NULL,
    to_id integer,
    from_id integer,
    content_id integer,
    other_id integer,
    message_template character varying(64),
    props bytea,
    created timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.log_email OWNER TO zotonic;

--
-- Name: log_email_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE log_email_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.log_email_id_seq OWNER TO zotonic;

--
-- Name: log_email_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE log_email_id_seq OWNED BY log_email.id;


--
-- Name: log_email_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('log_email_id_seq', 1, false);


--
-- Name: log_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE log_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.log_id_seq OWNER TO zotonic;

--
-- Name: log_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE log_id_seq OWNED BY log.id;


--
-- Name: log_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('log_id_seq', 8, true);


--
-- Name: medium; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE medium (
    id integer NOT NULL,
    filename character varying(400),
    rootname character varying(100),
    mime character varying(128) DEFAULT 'application/octet-stream'::character varying NOT NULL,
    width integer DEFAULT 0 NOT NULL,
    height integer DEFAULT 0 NOT NULL,
    orientation integer DEFAULT 1 NOT NULL,
    sha1 character varying(40),
    size integer DEFAULT 0 NOT NULL,
    preview_filename character varying(400),
    preview_width integer DEFAULT 0 NOT NULL,
    preview_height integer DEFAULT 0 NOT NULL,
    is_deletable_file boolean DEFAULT false NOT NULL,
    is_deletable_preview boolean DEFAULT false NOT NULL,
    props bytea,
    created timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.medium OWNER TO zotonic;

--
-- Name: medium_deleted; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE medium_deleted (
    id integer NOT NULL,
    filename character varying(400) NOT NULL,
    deleted timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.medium_deleted OWNER TO zotonic;

--
-- Name: medium_deleted_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE medium_deleted_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.medium_deleted_id_seq OWNER TO zotonic;

--
-- Name: medium_deleted_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE medium_deleted_id_seq OWNED BY medium_deleted.id;


--
-- Name: medium_deleted_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('medium_deleted_id_seq', 1, false);


--
-- Name: module; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE module (
    id integer NOT NULL,
    name character varying(80) DEFAULT ''::character varying NOT NULL,
    uri character varying(250) DEFAULT ''::character varying NOT NULL,
    is_active boolean DEFAULT false NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone DEFAULT now() NOT NULL,
    schema_version integer
);


ALTER TABLE public.module OWNER TO zotonic;

--
-- Name: module_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE module_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.module_id_seq OWNER TO zotonic;

--
-- Name: module_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE module_id_seq OWNED BY module.id;


--
-- Name: module_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('module_id_seq', 27, true);


--
-- Name: oauth_application_perm; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE oauth_application_perm (
    application_id integer NOT NULL,
    perm character varying(64) NOT NULL
);


ALTER TABLE public.oauth_application_perm OWNER TO zotonic;

--
-- Name: oauth_application_registry; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE oauth_application_registry (
    id integer NOT NULL,
    user_id integer,
    consumer_key character varying(64) NOT NULL,
    consumer_secret character varying(64) NOT NULL,
    enabled boolean DEFAULT true NOT NULL,
    callback_uri character varying(255) NOT NULL,
    application_uri character varying(255) NOT NULL,
    application_title character varying(80) NOT NULL,
    application_descr text NOT NULL,
    application_notes text NOT NULL,
    application_type character varying(20) NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.oauth_application_registry OWNER TO zotonic;

--
-- Name: oauth_application_registry_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE oauth_application_registry_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.oauth_application_registry_id_seq OWNER TO zotonic;

--
-- Name: oauth_application_registry_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE oauth_application_registry_id_seq OWNED BY oauth_application_registry.id;


--
-- Name: oauth_application_registry_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('oauth_application_registry_id_seq', 1, false);


--
-- Name: oauth_application_token; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE oauth_application_token (
    id integer NOT NULL,
    application_id integer NOT NULL,
    user_id integer NOT NULL,
    token character varying(64) NOT NULL,
    token_secret character varying(64) NOT NULL,
    token_type oauth_token_type,
    authorized boolean DEFAULT false NOT NULL,
    callback_uri character varying(255) NOT NULL,
    token_ttl timestamp with time zone DEFAULT '9999-12-31 00:00:00'::timestamp without time zone NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.oauth_application_token OWNER TO zotonic;

--
-- Name: oauth_application_token_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE oauth_application_token_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.oauth_application_token_id_seq OWNER TO zotonic;

--
-- Name: oauth_application_token_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE oauth_application_token_id_seq OWNED BY oauth_application_token.id;


--
-- Name: oauth_application_token_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('oauth_application_token_id_seq', 1, false);


--
-- Name: oauth_nonce; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE oauth_nonce (
    id integer NOT NULL,
    consumer_key character varying(64) NOT NULL,
    token character varying(64) NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL,
    nonce character varying(80) NOT NULL
);


ALTER TABLE public.oauth_nonce OWNER TO zotonic;

--
-- Name: oauth_nonce_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE oauth_nonce_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.oauth_nonce_id_seq OWNER TO zotonic;

--
-- Name: oauth_nonce_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE oauth_nonce_id_seq OWNED BY oauth_nonce.id;


--
-- Name: oauth_nonce_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('oauth_nonce_id_seq', 1, false);


--
-- Name: persistent; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE persistent (
    id character varying(32) NOT NULL,
    props bytea,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.persistent OWNER TO zotonic;

--
-- Name: pivot_task_queue; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE pivot_task_queue (
    id integer NOT NULL,
    module character varying(30) NOT NULL,
    function character varying(30) NOT NULL,
    key character varying(100) DEFAULT ''::character varying NOT NULL,
    due timestamp without time zone,
    props bytea
);


ALTER TABLE public.pivot_task_queue OWNER TO zotonic;

--
-- Name: pivot_task_queue_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE pivot_task_queue_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pivot_task_queue_id_seq OWNER TO zotonic;

--
-- Name: pivot_task_queue_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE pivot_task_queue_id_seq OWNED BY pivot_task_queue.id;


--
-- Name: pivot_task_queue_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('pivot_task_queue_id_seq', 2, true);


--
-- Name: predicate_category; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE predicate_category (
    id integer NOT NULL,
    is_subject boolean DEFAULT true NOT NULL,
    predicate_id integer NOT NULL,
    category_id integer NOT NULL
);


ALTER TABLE public.predicate_category OWNER TO zotonic;

--
-- Name: predicate_category_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE predicate_category_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.predicate_category_id_seq OWNER TO zotonic;

--
-- Name: predicate_category_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE predicate_category_id_seq OWNED BY predicate_category.id;


--
-- Name: predicate_category_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('predicate_category_id_seq', 19, true);


--
-- Name: protect; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE protect (
    id integer NOT NULL
);


ALTER TABLE public.protect OWNER TO zotonic;

--
-- Name: rsc; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE rsc (
    id integer NOT NULL,
    uri character varying(250),
    name character varying(80),
    page_path character varying(80),
    is_authoritative boolean DEFAULT true NOT NULL,
    is_published boolean DEFAULT false NOT NULL,
    is_featured boolean DEFAULT false NOT NULL,
    is_protected boolean DEFAULT false NOT NULL,
    publication_start timestamp with time zone DEFAULT now() NOT NULL,
    publication_end timestamp with time zone DEFAULT '9999-06-01 00:00:00+00'::timestamp with time zone NOT NULL,
    creator_id integer,
    modifier_id integer,
    version integer DEFAULT 1 NOT NULL,
    category_id integer NOT NULL,
    visible_for integer DEFAULT 1 NOT NULL,
    slug character varying(80) DEFAULT ''::character varying NOT NULL,
    props bytea,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone DEFAULT now() NOT NULL,
    pivot_category_nr integer,
    pivot_tsv tsvector,
    pivot_rtsv tsvector,
    pivot_first_name character varying(100),
    pivot_surname character varying(100),
    pivot_gender character varying(1),
    pivot_date_start timestamp with time zone,
    pivot_date_end timestamp with time zone,
    pivot_date_start_month_day integer,
    pivot_date_end_month_day integer,
    pivot_street character varying(120),
    pivot_city character varying(100),
    pivot_state character varying(50),
    pivot_postcode character varying(30),
    pivot_country character varying(80),
    pivot_geocode bigint,
    pivot_geocode_qhash bytea,
    pivot_title character varying(100)
);


ALTER TABLE public.rsc OWNER TO zotonic;

--
-- Name: COLUMN rsc.visible_for; Type: COMMENT; Schema: public; Owner: zotonic
--

COMMENT ON COLUMN rsc.visible_for IS '0 = public, 1 = community, 2 = group';


--
-- Name: rsc_gone; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE rsc_gone (
    id bigint NOT NULL,
    new_id bigint,
    new_uri character varying(250),
    version integer NOT NULL,
    uri character varying(250),
    name character varying(80),
    page_path character varying(80),
    is_authoritative boolean DEFAULT true NOT NULL,
    creator_id bigint,
    modifier_id bigint,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.rsc_gone OWNER TO zotonic;

--
-- Name: rsc_id_seq; Type: SEQUENCE; Schema: public; Owner: zotonic
--

CREATE SEQUENCE rsc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.rsc_id_seq OWNER TO zotonic;

--
-- Name: rsc_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: zotonic
--

ALTER SEQUENCE rsc_id_seq OWNED BY rsc.id;


--
-- Name: rsc_id_seq; Type: SEQUENCE SET; Schema: public; Owner: zotonic
--

SELECT pg_catalog.setval('rsc_id_seq', 318, true);


--
-- Name: rsc_pivot_queue; Type: TABLE; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE TABLE rsc_pivot_queue (
    rsc_id integer NOT NULL,
    serial integer DEFAULT 1 NOT NULL,
    due timestamp without time zone NOT NULL,
    is_update boolean DEFAULT true NOT NULL
);


ALTER TABLE public.rsc_pivot_queue OWNER TO zotonic;

--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY comment ALTER COLUMN id SET DEFAULT nextval('comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY config ALTER COLUMN id SET DEFAULT nextval('config_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY edge ALTER COLUMN id SET DEFAULT nextval('edge_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY emailq ALTER COLUMN id SET DEFAULT nextval('emailq_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY identity ALTER COLUMN id SET DEFAULT nextval('identity_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY log ALTER COLUMN id SET DEFAULT nextval('log_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY log_email ALTER COLUMN id SET DEFAULT nextval('log_email_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY medium_deleted ALTER COLUMN id SET DEFAULT nextval('medium_deleted_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY module ALTER COLUMN id SET DEFAULT nextval('module_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY oauth_application_registry ALTER COLUMN id SET DEFAULT nextval('oauth_application_registry_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY oauth_application_token ALTER COLUMN id SET DEFAULT nextval('oauth_application_token_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY oauth_nonce ALTER COLUMN id SET DEFAULT nextval('oauth_nonce_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY pivot_task_queue ALTER COLUMN id SET DEFAULT nextval('pivot_task_queue_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY predicate_category ALTER COLUMN id SET DEFAULT nextval('predicate_category_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY rsc ALTER COLUMN id SET DEFAULT nextval('rsc_id_seq'::regclass);


--
-- Data for Name: category; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY category (id, parent_id, seq, nr, lvl, lft, rght, props) FROM stdin;
115	\N	20	20	1	20	23	\\x01326a3a836c0000000168026400047061746868026400026f6b6a6a
315	115	23	23	2	23	23	\\x01326a3a836c0000000168026400047061746868026400026f6b6b0001736a
117	115	22	22	2	22	22	\\x01326a3a836c0000000168026400047061746868026400026f6b6b0001736a
116	115	21	21	2	21	21	\\x01326a3a836c0000000168026400047061746868026400026f6b6b0001736a
122	\N	17	17	1	17	19	\\x01326a3a836c0000000168026400047061746868026400026f6b6a6a
313	122	19	19	2	19	19	\\x01326a3a836c0000000168026400047061746868026400026f6b6b00017a6a
123	122	18	18	2	18	18	\\x01326a3a836c0000000168026400047061746868026400026f6b6b00017a6a
120	\N	15	15	1	15	16	\\x01326a3a836c0000000168026400047061746868026400026f6b6a6a
121	120	16	16	2	16	16	\\x01326a3a836c0000000168026400047061746868026400026f6b6b0001786a
110	\N	10	10	1	10	14	\\x01326a3a836c0000000168026400047061746868026400026f6b6a6a
114	110	14	14	2	14	14	\\x01326a3a836c0000000168026400047061746868026400026f6b6b00016e6a
113	110	13	13	2	13	13	\\x01326a3a836c0000000168026400047061746868026400026f6b6b00016e6a
112	110	12	12	2	12	12	\\x01326a3a836c0000000168026400047061746868026400026f6b6b00016e6a
111	110	11	11	2	11	11	\\x01326a3a836c0000000168026400047061746868026400026f6b6b00016e6a
103	\N	9	9	1	9	9	\\x01326a3a836c0000000168026400047061746868026400026f6b6a6a
108	\N	8	8	1	8	8	\\x01326a3a836c0000000168026400047061746868026400026f6b6a6a
119	\N	6	6	1	6	7	\\x01326a3a836c0000000168026400047061746868026400026f6b6a6a
107	119	7	7	2	7	7	\\x01326a3a836c0000000168026400047061746868026400026f6b6b0001776a
102	\N	5	5	1	5	5	\\x01326a3a836c0000000168026400047061746868026400026f6b6a6a
104	\N	2	2	1	2	4	\\x01326a3a836c0000000168026400047061746868026400026f6b6a6a
106	104	3	3	2	3	4	\\x01326a3a836c0000000168026400047061746868026400026f6b6b0001686a
109	106	4	4	3	4	4	\\x01326a3a836c0000000168026400047061746868026400026f6b6b0002686a6a
101	\N	1	1	1	1	1	\\x01326a3a836c0000000168026400047061746868026400026f6b6a6a
\.


--
-- Data for Name: comment; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY comment (id, is_visible, rsc_id, user_id, persistent_id, gravatar_code, email, name, user_agent, ip_address, keep_informed, props, created) FROM stdin;
\.


--
-- Data for Name: config; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY config (id, module, key, value, props, created, modified) FROM stdin;
1	zotonic	version	0.10-dev	\N	2012-12-17 21:09:23.016035+00	2012-12-17 21:09:23.016035+00
3	i18n	language_list		\\x01326a3a836c0000000168026400046c6973746c00000009680264000261726c0000000268026400086c616e67756167656d0000000ed8a7d984d8b9d8b1d8a8d98ad8a9680264000a69735f656e61626c656464000566616c73656a6802640002656e6c0000000268026400086c616e67756167656d00000007456e676c697368680264000a69735f656e61626c6564640004747275656a680264000265746c0000000268026400086c616e67756167656d000000054565737469680264000a69735f656e61626c6564640004747275656a680264000265736c0000000268026400086c616e67756167656d0000000845737061c3b16f6c680264000a69735f656e61626c6564640004747275656a680264000266726c0000000268026400086c616e67756167656d000000094672616ec3a7616973680264000a69735f656e61626c6564640004747275656a680264000264656c0000000268026400086c616e67756167656d0000000744657574736368680264000a69735f656e61626c6564640004747275656a68026400026e6c6c0000000268026400086c616e67756167656d0000000a4e656465726c616e6473680264000a69735f656e61626c6564640004747275656a680264000274726c0000000268026400086c616e67756167656d0000000854c3bc726bc3a765680264000a69735f656e61626c6564640004747275656a6802640002706c6c0000000268026400086c616e67756167656d00000006506f6c736b69680264000a69735f656e61626c6564640004747275656a6a6a	2012-12-17 21:09:23.654975+00	2012-12-17 21:09:23.654975+00
4	site	sign_key	G03Qa8l0CGBTiwBYPcH0aqg3xMhZdkzUERD9PZSD1Nd6IAvKFO	\N	2012-12-17 21:10:51.377186+00	2012-12-17 21:10:51.377186+00
2	m_category	meta		\\x01326a3a836c00000001680264000a747265655f646972747964000566616c73656a	2012-12-17 21:09:23.180804+00	2012-12-17 21:09:23.180804+00
\.


--
-- Data for Name: edge; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY edge (id, subject_id, predicate_id, object_id, seq, creator_id, created) FROM stdin;
\.


--
-- Data for Name: emailq; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY emailq (id, status, retry_on, retry, sender, recipient, props, sent, created) FROM stdin;
\.


--
-- Data for Name: identity; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY identity (id, rsc_id, type, key, is_unique, is_verified, verify_key, propb, prop1, prop2, prop3, created, modified, visited) FROM stdin;
2	318	username_pw	editor	t	t	\N	\\x01326a3a836803640004686173686b000a31435a637370337237676d00000014c287a9f9c65ed6fdc6cfc3868882aa606b4cc79a				2012-12-17 21:12:26.841582+00	2012-12-17 21:12:26.841582+00	2012-12-17 21:12:33.238414+00
1	1	username_pw	admin	t	f	\N	\\x01326a3a836803640004686173686b000a3579773373646f454e366d000000148912d8e814b8fc9d5c3886d135c6f16d5146c4be				2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	2012-12-17 21:12:51.234617+00
\.


--
-- Data for Name: log; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY log (id, rsc_id, user_id, type, module, props, created) FROM stdin;
1	\N	1	info	z_datamodel	\\x01326a3a836c0000000268026400076d6573736167656d0000001d4372656174696e67206e657720746578742027706167655f686f6d652768026400046c696e6561986a	2012-12-17 21:09:23.454205+00
2	\N	1	info	z_datamodel	\\x01326a3a836c0000000268026400076d6573736167656d0000001c4372656174696e67206e65772063617465676f727920276d656e752768026400046c696e6561986a	2012-12-17 21:09:23.55514+00
3	\N	1	info	z_datamodel	\\x01326a3a836c0000000268026400076d6573736167656d0000001d4372656174696e67206e6577206d656e7520276d61696e5f6d656e752768026400046c696e6561986a	2012-12-17 21:09:23.585119+00
4	\N	\N	info	m_category	\\x01326a3a836c0000000268026400076d6573736167656d0000001f43617465676f72792072656e756d626572696e6720636f6d706c657465642e68026400046c696e6562000002ae6a	2012-12-17 21:09:53.012084+00
5	\N	1	info	z_datamodel	\\x01326a3a836c0000000268026400076d6573736167656d000000204372656174696e67206e65772063617465676f7279202761636c5f726f6c652768026400046c696e6561986a	2012-12-17 21:11:24.03545+00
6	\N	1	info	z_datamodel	\\x01326a3a836c0000000268026400076d6573736167656d000000284372656174696e67206e657720707265646963617465202761636c5f726f6c655f6d656d6265722768026400046c696e6561986a	2012-12-17 21:11:24.11458+00
7	\N	1	info	z_datamodel	\\x01326a3a836c0000000268026400076d6573736167656d000000234372656174696e67206e65772061636c5f726f6c652027726f6c655f6d656d6265722768026400046c696e6561986a	2012-12-17 21:11:24.137643+00
8	\N	\N	info	m_category	\\x01326a3a836c0000000268026400076d6573736167656d0000001f43617465676f72792072656e756d626572696e6720636f6d706c657465642e68026400046c696e6562000002ae6a	2012-12-17 21:11:42.999098+00
\.


--
-- Data for Name: log_email; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY log_email (id, severity, message_nr, mailer_status, mailer_message, mailer_host, envelop_to, envelop_from, to_id, from_id, content_id, other_id, message_template, props, created) FROM stdin;
\.


--
-- Data for Name: medium; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY medium (id, filename, rootname, mime, width, height, orientation, sha1, size, preview_filename, preview_width, preview_height, is_deletable_file, is_deletable_preview, props, created) FROM stdin;
\.


--
-- Data for Name: medium_deleted; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY medium_deleted (id, filename, deleted) FROM stdin;
\.


--
-- Data for Name: module; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY module (id, name, uri, is_active, created, modified, schema_version) FROM stdin;
2	mod_base		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
3	mod_base_site		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
5	mod_oauth		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
6	mod_search		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
7	mod_oembed		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
8	mod_atom_feed		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
9	mod_translation		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
10	mod_signal		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
11	mod_logging		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
12	mod_seo		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
13	mod_seo_google		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
14	mod_seo_sitemap		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
15	mod_authentication		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
17	mod_admin		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
18	mod_admin_category		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
19	mod_admin_config		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
20	mod_admin_identity		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
21	mod_admin_modules		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
22	mod_admin_predicate		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
23	mod_l10n		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
24	mod_geomap		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
25	mod_comment		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
26	mod_bootstrap		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	\N
1	zotonicdemo		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	1
4	mod_menu		t	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	1
27	mod_acl_simple_roles		t	2012-12-17 21:11:23.996872+00	2012-12-17 21:11:23.996872+00	1
16	mod_acl_adminonly		f	2012-12-17 21:09:21.872072+00	2012-12-17 21:11:27.521089+00	\N
\.


--
-- Data for Name: oauth_application_perm; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY oauth_application_perm (application_id, perm) FROM stdin;
\.


--
-- Data for Name: oauth_application_registry; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY oauth_application_registry (id, user_id, consumer_key, consumer_secret, enabled, callback_uri, application_uri, application_title, application_descr, application_notes, application_type, "timestamp") FROM stdin;
\.


--
-- Data for Name: oauth_application_token; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY oauth_application_token (id, application_id, user_id, token, token_secret, token_type, authorized, callback_uri, token_ttl, "timestamp") FROM stdin;
\.


--
-- Data for Name: oauth_nonce; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY oauth_nonce (id, consumer_key, token, "timestamp", nonce) FROM stdin;
\.


--
-- Data for Name: persistent; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY persistent (id, props, created, modified) FROM stdin;
kbmfUct11KrdkEN1B2Ov	\\x01326a3a836c0000000168026400086c616e67756167656400026e6c6a	2012-12-17 21:12:51.25348+00	2012-12-17 21:12:51.25348+00
\.


--
-- Data for Name: pivot_task_queue; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY pivot_task_queue (id, module, function, key, due, props) FROM stdin;
\.


--
-- Data for Name: predicate_category; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY predicate_category (id, is_subject, predicate_id, category_id) FROM stdin;
1	t	300	104
2	f	301	102
3	f	304	110
4	t	308	104
5	t	308	102
6	t	308	119
7	t	308	108
8	t	308	103
9	t	308	110
10	t	308	114
11	f	308	123
12	t	309	102
13	t	309	103
14	t	309	104
15	t	309	119
16	f	309	114
17	t	310	120
18	t	316	315
19	f	316	102
\.


--
-- Data for Name: protect; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY protect (id) FROM stdin;
116
115
117
101
102
110
111
112
113
114
122
123
1
315
316
317
300
301
303
304
308
309
310
312
313
314
\.


--
-- Data for Name: rsc; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY rsc (id, uri, name, page_path, is_authoritative, is_published, is_featured, is_protected, publication_start, publication_end, creator_id, modifier_id, version, category_id, visible_for, slug, props, created, modified, pivot_category_nr, pivot_tsv, pivot_rtsv, pivot_first_name, pivot_surname, pivot_gender, pivot_date_start, pivot_date_end, pivot_date_start_month_day, pivot_date_end_month_day, pivot_street, pivot_city, pivot_state, pivot_postcode, pivot_country, pivot_geocode, pivot_geocode_qhash, pivot_title) FROM stdin;
315	\N	acl_role	\N	t	t	f	t	2012-12-17 21:11:24.040949+00	9999-06-01 00:00:00+00	1	1	1	116	0	acl-role	\\x01326a3a836c000000056802640008646174655f656e64640009756e646566696e6564680264000a646174655f7374617274640009756e646566696e656468026400057469746c656d0000000841434c20526f6c65680264000d6d616e616765645f70726f70736c0000000168026400057469746c656d0000000841434c20526f6c656a680264000c696e7374616c6c65645f62796400146d6f645f61636c5f73696d706c655f726f6c65736a	2012-12-17 21:11:24.040949+00	2012-12-17 21:11:24+00	21	'acl':1A,3C,6C,8C 'acl-rol':5C 'categori':11C 'meta':10C 'role':2A,4C,7C,9C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	acl role
103	http://purl.org/dc/dcmitype/PhysicalObject	artifact	\N	t	t	f	f	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d00000008417274696661637468026400026e6c6d0000000841727465666163746a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'artefact':2A 'artifact':1A,3C 'categori':5C 'meta':4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	artifact
107	\N	website	\N	t	t	f	f	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d000000075765627369746568026400026e6c6d00000007576562736974656a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'meta':4C 'websit':1A,2A,3C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	website
108	http://purl.org/dc/dcmitype/Event	event	\N	t	t	f	f	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d000000054576656e7468026400026e6c6d000000094576656e656d656e746a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'evenement':2A 'event':1A,3C 'meta':4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	event
109	\N	news	\N	t	t	f	f	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d000000044e65777368026400026e6c6d000000064e69657577736a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'meta':4C 'news':1A,3C 'nieuw':2A	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	news
110	http://purl.org/dc/dcmitype/Image	media	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d000000054d6564696168026400026e6c6d000000054d656469616a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'media':1A,2A,3C 'meta':4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	media
111	http://purl.org/dc/dcmitype/StillImage	image	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d00000005496d61676568026400026e6c6d0000000a41666265656c64696e676a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'afbeeld':2A 'categori':5C 'imag':1A,3C 'meta':4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	image
112	http://purl.org/dc/dcmitype/MovingImage	video	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d00000005566964656f68026400026e6c6d00000005566964656f6a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'meta':4C 'video':1A,2A,3C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	video
113	http://purl.org/dc/dcmitype/Sound	audio	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d00000005417564696f68026400026e6c6d0000000647656c7569646a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'audio':1A,3C 'categori':5C 'geluid':2A 'meta':4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	audio
114	\N	document	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d00000008446f63756d656e7468026400026e6c6d00000008446f63756d656e746a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'document':1A,2A,3C 'meta':4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	document
119	\N	location	\N	t	t	f	f	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d000000084c6f636174696f6e68026400026e6c6d000000074c6f63617469656a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'locat':1A,3C 'locatie':2A 'meta':4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	location
120	http://purl.org/dc/dcmitype/Collection	collection	\N	t	t	f	f	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d0000000a436f6c6c656374696f6e68026400026e6c6d00000009436f6c6c65637469656a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'collect':1A,3C 'collectie':2A 'meta':4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	collection
121	http://purl.org/dc/dcmitype/Dataset	query	\N	t	t	f	f	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d0000000c53656172636820717565727968026400026e6c6d0000000c5a6f656b6f706472616368746a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':6C 'meta':5C 'queri':2A,4C 'search':1A 'zoekopdracht':3A	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	search query
1	\N	administrator	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	2	102	0		\\x01326a3a836c0000000268026400057469746c656d00000012536974652041646d696e6973747261746f72680264000d707265665f6c616e67756167656400026e6c6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:11:03+00	5	'administr':2A,4C,5C 'person':6C 'site':1A,3C	'zpc102':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	site administrator
316	\N	acl_role_member	\N	t	t	f	t	2012-12-17 21:11:24.105801+00	9999-06-01 00:00:00+00	1	1	1	117	0	acl-role-member	\\x01326a3a836c000000056802640008646174655f656e64640009756e646566696e6564680264000a646174655f7374617274640009756e646566696e656468026400057469746c656d0000000f41434c20526f6c65204d656d626572680264000d6d616e616765645f70726f70736c0000000168026400057469746c656d0000000f41434c20526f6c65204d656d6265726a680264000c696e7374616c6c65645f62796400146d6f645f61636c5f73696d706c655f726f6c65736a	2012-12-17 21:11:24.105801+00	2012-12-17 21:11:24+00	22	'acl':1A,4C,8C,11C 'acl-role-memb':7C 'member':3A,6C,10C,13C 'meta':14C 'predic':15C 'role':2A,5C,9C,12C	'zpc115':2 'zpc117':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	acl role member
101	\N	other	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d0000000d556e63617465676f72697a656468026400026e6c6d000000105a6f6e6465722063617465676f7269656a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':6C 'categorie':3A 'meta':5C 'uncategor':1A	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	uncategorized
102	\N	person	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d00000006506572736f6e68026400026e6c6d00000007506572736f6f6e6a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'meta':4C 'person':1A,2A,3C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	person
104	http://purl.org/dc/dcmitype/Text	text	\N	t	t	f	f	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d000000045465787468026400026e6c6d0000000554656b73746a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'meta':4C 'tekst':2A 'text':1A,3C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	text
106	\N	article	\N	t	t	f	f	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d0000000741727469636c6568026400026e6c6d00000007417274696b656c6a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'articl':1A,3C 'artikel':2A 'categori':5C 'meta':4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	article
115	\N	meta	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d000000044d65746168026400026e6c6d000000044d6574616a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'meta':1A,2A,3C,4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	meta
117	\N	predicate	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d0000000950726564696361746568026400026e6c6d0000000950726564696b6161746a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'meta':4C 'predic':1A,3C 'predikat':2A	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	predicate
312	\N	page_home	\N	t	t	f	t	2012-12-17 21:09:23.455838+00	9999-06-01 00:00:00+00	1	1	1	104	0	zotonic-demonstration-site	\\x01326a3a836c000000076802640008646174655f656e64640009756e646566696e6564680264000a646174655f7374617274640009756e646566696e656468026400057469746c656d0000001a5a6f746f6e69632044656d6f6e7374726174696f6e2073697465680264000773756d6d6172796d000000585468697320736974652066756e6374696f6e7320617320612064656d6f6e7374726174696f6e207369746520666f72205a6f746f6e696320616e6420746865205a6f746f6e69632061646d696e20696e746572666163652e6802640004626f64796d000000523c703e546f2065646974207468697320706167652c20636c69636b2074686520627574746f6e2062656c6f7720746f206c6f6720696e20746f207468652061646d696e20696e746572666163652e3c2f703e680264000d6d616e616765645f70726f70736c0000000368026400057469746c656d0000001a5a6f746f6e69632044656d6f6e7374726174696f6e2073697465680264000773756d6d6172796d000000585468697320736974652066756e6374696f6e7320617320612064656d6f6e7374726174696f6e207369746520666f72205a6f746f6e696320616e6420746865205a6f746f6e69632061646d696e20696e746572666163652e6802640004626f64796d000000523c703e546f2065646974207468697320706167652c20636c69636b2074686520627574746f6e2062656c6f7720746f206c6f6720696e20746f207468652061646d696e20696e746572666163652e3c2f703e6a680264000c696e7374616c6c65645f627964000b7a6f746f6e696364656d6f6a	2012-12-17 21:09:23.455838+00	2012-12-17 21:09:23+00	2	'admin':17C,31C 'button':10C 'click':8C 'demonstr':2A,24C,34C,38C 'edit':5C 'function':21C 'home':41C 'interfac':18C,32C 'log':13C 'page':7C,40C 'site':3A,20C,25C,35C,39C 'text':42C 'zoton':1A,27C,30C,33C,37C 'zotonic-demonstration-sit':36C	'zpc104':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	zotonic demonstration site
314	\N	main_menu	\N	t	t	f	t	2012-12-17 21:09:23.508541+00	9999-06-01 00:00:00+00	1	1	1	313	0	main-menu	\\x01326a3a836c000000066802640008646174655f656e64640009756e646566696e6564680264000a646174655f7374617274640009756e646566696e656468026400057469746c656d000000094d61696e206d656e7568026400046d656e756d00000000680264000d6d616e616765645f70726f70736c0000000268026400057469746c656d000000094d61696e206d656e7568026400046d656e756d000000006a680264000c696e7374616c6c65645f62796400086d6f645f6d656e756a	2012-12-17 21:09:23.508541+00	2012-12-17 21:09:23+00	19	'categor':10C 'main':1A,3C,6C,8C 'main-menu':5C 'menu':2A,4C,7C,9C,11C	'zpc122':2 'zpc313':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	main menu
317	http://demo.zotonic.com/nl/id/317	role_member	\N	t	t	f	t	2012-12-17 21:11:24.13334+00	9999-06-01 00:00:00+00	1	1	2	315	1	acl-role-for-members	\\x01326a3a836c0000001f68026400057469746c6568026400057472616e736c0000000168026400026e6c6d0000001441434c20726f6c6520666f72206d656d626572736a680264000773756d6d61727968026400057472616e736c0000000168026400026e6c6d000000a854686520726967687473206f66207468697320726f6c65206172652061737369676e656420746f206d656d6265727320286c6f67676564206f6e29207768656e207468657920617265206e6f74206d656d626572206f6620616e79206f746865722041434c20726f6c652e20204d616b65207468652075736572206d656d626572206f6620616e6f7468657220726f6c6520746f206f76657272756c65207468697320726f6c652e6a680264000361636c6c00000007680264000a63617465676f726965736c0000000364000761727469636c656400056d65646961640005696d6167656a68026400076d6f64756c65736c000000036400096d6f645f61646d696e6400086d6f645f6d656e7564000a6d6f645f6f656d6265646a6802640008766965775f616c6c64000566616c7365680264000f6f6e6c795f7570646174655f6f776e64000566616c7365680264001066696c655f75706c6f61645f73697a656200002728680264000966696c655f6d696d656c000000066d0000000a696d6167652f6a7065676d00000009696d6167652f706e676d00000009696d6167652f6769666d0000000f6170706c69636174696f6e2f7064666d00000007766964656f2f2a6d0000000a746578742f706c61696e6a680264000b76697369626c655f666f7261006a680264000d6d616e616765645f70726f70736c00000004680264000b76697369626c655f666f72610168026400057469746c656d0000001441434c20726f6c6520666f72206d656d62657273680264000773756d6d6172796d000000a854686520726967687473206f66207468697320726f6c65206172652061737369676e656420746f206d656d6265727320286c6f67676564206f6e29207768656e207468657920617265206e6f74206d656d626572206f6620616e79206f746865722041434c20726f6c652e20204d616b65207468652075736572206d656d626572206f6620616e6f7468657220726f6c6520746f206f76657272756c65207468697320726f6c652e680264000361636c6c000000066802640008766965775f616c6c64000566616c7365680264000f6f6e6c795f7570646174655f6f776e64000566616c7365680264001066696c655f75706c6f61645f73697a656200000400680264000966696c655f6d696d656c000000036b000a696d6167652f6a7065676b0009696d6167652f706e676b0009696d6167652f6769666a680264000a63617465676f726965736c0000000264000761727469636c65640005696d6167656a68026400076d6f64756c65736a6a6a680264000c696e7374616c6c65645f62796400146d6f645f61636c5f73696d706c655f726f6c65736802640006626c6f636b736a680264000b73686f72745f7469746c6568026400057472616e736c0000000168026400026e6c6d000000006a6802640004626f647968026400057472616e736c0000000168026400026e6c6d000000006a68026400086c616e67756167656c000000016400026e6c6a680264000570686f6e656d00000000680264000970686f6e655f616c746d00000000680264000f70686f6e655f656d657267656e63796d000000006802640005656d61696c6d000000006802640007776562736974656d00000000680264000f616464726573735f636f756e7472796d000000006802640010616464726573735f7374726565745f316d000000006802640010616464726573735f7374726565745f326d00000000680264000c616464726573735f636974796d000000006802640010616464726573735f706f7374636f64656d00000000680264000d616464726573735f73746174656d00000000680264000c6d61696c5f636f756e7472796d00000000680264000d6d61696c5f7374726565745f316d00000000680264000d6d61696c5f7374726565745f326d0000000068026400096d61696c5f636974796d00000000680264000d6d61696c5f706f7374636f64656d00000000680264000a6d61696c5f73746174656d00000000680264000b637573746f6d5f736c756764000566616c7365680264000973656f5f7469746c656d00000000680264000b73656f5f6e6f696e6465786d00000000680264000c73656f5f6b6579776f7264736d00000000680264000873656f5f646573636d000000006a	2012-12-17 21:11:24.13334+00	2012-12-17 21:12:10+00	23	'acl':1A,5C,10C,17C,38C 'acl-role-for-memb':9C 'another':45C 'any':36C 'are':24C,32C 'assigned':25C 'for':3A 'logged':28C 'mak':40C 'member':4A,8C,13C,15C,27C,34C,43C 'meta':16C 'not':33C 'on':29C 'other':37C 'overrul':48C 'right':20C 'rol':2A,23C,39C,46C,50C 'role':6C,11C,14C,18C 'the':19C,41C 'they':31C 'this':22C,49C 'to':26C,47C 'user':42C 'when':30C	'zpc115':2 'zpc315':1	\N	\N	\N	\N	\N	\N	\N						\N	\N	acl role for members
116	\N	category	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d0000000843617465676f727968026400026e6c6d0000000943617465676f7269656a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':1A,3C,5C 'categorie':2A 'meta':4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	category
122	\N	categorization	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d0000000e43617465676f72697a6174696f6e68026400026e6c6d0000000d43617465676f726973617469656a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categor':1A,3C 'categori':5C 'categorisatie':2A 'meta':4C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	categorization
123	\N	keyword	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	116	0		\\x01326a3a836c0000000168026400057469746c6568026400057472616e736c000000026802640002656e6d000000074b6579776f726468026400026e6c6d0000000954726566776f6f72646a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	21	'categori':5C 'keyword':1A,3C 'meta':4C 'trefwoord':2A	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	keyword
300	http://www.w3.org/1999/02/22-rdf-syntax-ns#about	about	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	117	0		\\x01326a3a836c000000026802640008726576657273656464000566616c736568026400057469746c6568026400057472616e736c000000026802640002656e6b000541626f757468026400026e6c6b00044f7665726a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	22	'meta':2C 'predic':3C	'zpc115':2 'zpc117':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	about
301	http://purl.org/dc/terms/creator	author	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	117	0		\\x01326a3a836c000000026802640008726576657273656464000566616c736568026400057469746c6568026400057472616e736c000000026802640002656e6b0006417574686f7268026400026e6c6b00064175746575726a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	22	'auteur':2A 'author':1A,3C 'meta':4C 'predic':5C	'zpc115':2 'zpc117':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	author
303	http://purl.org/dc/terms/relation	relation	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	117	0		\\x01326a3a836c000000026802640008726576657273656464000566616c736568026400057469746c6568026400057472616e736c000000026802640002656e6b000852656c6174696f6e68026400026e6c6b000752656c617469656a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	22	'meta':4C 'predic':5C 'relat':1A,3C 'relatie':2A	'zpc115':2 'zpc117':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	relation
304	http://xmlns.com/foaf/0.1/depiction	depiction	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	117	0		\\x01326a3a836c000000026802640008726576657273656464000566616c736568026400057469746c6568026400057472616e736c000000026802640002656e6b0009446570696374696f6e68026400026e6c6b000a41666265656c64696e676a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	22	'afbeeld':2A 'depict':1A,3C 'meta':4C 'predic':5C	'zpc115':2 'zpc117':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	depiction
308	http://purl.org/dc/elements/1.1/subject	subject	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	117	0		\\x01326a3a836c000000026802640008726576657273656464000566616c736568026400057469746c6568026400057472616e736c000000026802640002656e6b00074b6579776f726468026400026e6c6b000954726566776f6f72646a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	22	'keyword':1A 'meta':4C 'predic':5C 'subject':3C 'trefwoord':2A	'zpc115':2 'zpc117':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	keyword
309	http://zotonic.net/predicate/hasDocument	hasdocument	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	117	0		\\x01326a3a836c000000026802640008726576657273656464000566616c736568026400057469746c6568026400057472616e736c000000026802640002656e6b0008446f63756d656e7468026400026e6c6b0008446f63756d656e746a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	22	'document':1A,2A 'hasdocu':3C 'meta':4C 'predic':5C	'zpc115':2 'zpc117':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	document
310	http://purl.org/dc/terms/hasPart	haspart	\N	t	t	f	t	2012-12-17 21:09:21.872072+00	9999-06-01 00:00:00+00	1	1	1	117	0		\\x01326a3a836c000000026802640008726576657273656464000566616c736568026400057469746c6568026400057472616e736c000000026802640002656e6b0008436f6e7461696e7368026400026e6c6b000542657661746a6a	2012-12-17 21:09:21.872072+00	2012-12-17 21:09:21.872072+00	22	'bevat':2A 'contain':1A 'haspart':3C 'meta':4C 'predic':5C	'zpc115':2 'zpc117':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	contains
313	\N	menu	\N	t	t	f	t	2012-12-17 21:09:23.508541+00	9999-06-01 00:00:00+00	1	1	1	116	0	page-menu	\\x01326a3a836c000000056802640008646174655f656e64640009756e646566696e6564680264000a646174655f7374617274640009756e646566696e656468026400057469746c656d0000000950616765206d656e75680264000d6d616e616765645f70726f70736c0000000168026400057469746c656d0000000950616765206d656e756a680264000c696e7374616c6c65645f62796400086d6f645f6d656e756a	2012-12-17 21:09:23.508541+00	2012-12-17 21:09:23+00	21	'categori':10C 'menu':2A,4C,7C,8C 'meta':9C 'page':1A,3C,6C 'page-menu':5C	'zpc115':2 'zpc116':1	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	page menu
318	\N	\N	\N	t	t	f	f	2012-12-17 21:12:26.841582+00	9999-06-01 00:00:00+00	318	318	2	102	0	john-e-doe	\\x01326a3a836c000000066802640005656d61696c6d000000106a6f686e406578616d706c652e636f6d680264000c6e616d655f7375726e616d656d00000003446f6568026400136e616d655f7375726e616d655f7072656669786d00000002452e680264000a6e616d655f66697273746d000000044a6f686e68026400057469746c656d0000000b4a6f686e20452e20446f65680264000d707265665f6c616e67756167656400026e6c6a	2012-12-17 21:12:26.841582+00	2012-12-17 21:12:33+00	5	'doe':3A,5A,9C,13C 'e':2A,6C,8C,12C 'john':1A,4A,7C,11C 'john-e-do':10C 'john@example.com':15C 'person':14C	'zpc102':1	john	doe	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	\N	john e. doe
\.


--
-- Data for Name: rsc_gone; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY rsc_gone (id, new_id, new_uri, version, uri, name, page_path, is_authoritative, creator_id, modifier_id, created, modified) FROM stdin;
\.


--
-- Data for Name: rsc_pivot_queue; Type: TABLE DATA; Schema: public; Owner: zotonic
--

COPY rsc_pivot_queue (rsc_id, serial, due, is_update) FROM stdin;
\.


--
-- Name: auth_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY identity
    ADD CONSTRAINT auth_pkey PRIMARY KEY (id);


--
-- Name: category_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY category
    ADD CONSTRAINT category_pkey PRIMARY KEY (id);


--
-- Name: comment_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_pkey PRIMARY KEY (id);


--
-- Name: config_module_key_key; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY config
    ADD CONSTRAINT config_module_key_key UNIQUE (module, key);


--
-- Name: config_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY config
    ADD CONSTRAINT config_pkey PRIMARY KEY (id);


--
-- Name: edge_ops_key; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY edge
    ADD CONSTRAINT edge_ops_key UNIQUE (object_id, predicate_id, subject_id);


--
-- Name: edge_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY edge
    ADD CONSTRAINT edge_pkey PRIMARY KEY (id);


--
-- Name: edge_spo_key; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY edge
    ADD CONSTRAINT edge_spo_key UNIQUE (subject_id, predicate_id, object_id);


--
-- Name: email_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY emailq
    ADD CONSTRAINT email_pkey PRIMARY KEY (id);


--
-- Name: identity_type_key_unique; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY identity
    ADD CONSTRAINT identity_type_key_unique UNIQUE (type, key, is_unique);


--
-- Name: identity_verify_key_unique; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY identity
    ADD CONSTRAINT identity_verify_key_unique UNIQUE (verify_key);


--
-- Name: log_email_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY log_email
    ADD CONSTRAINT log_email_pkey PRIMARY KEY (id);


--
-- Name: log_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY log
    ADD CONSTRAINT log_pkey PRIMARY KEY (id);


--
-- Name: medium_deleted_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY medium_deleted
    ADD CONSTRAINT medium_deleted_pkey PRIMARY KEY (id);


--
-- Name: medium_filename_key; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY medium
    ADD CONSTRAINT medium_filename_key UNIQUE (filename);


--
-- Name: medium_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY medium
    ADD CONSTRAINT medium_pkey PRIMARY KEY (id);


--
-- Name: module_name_key; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY module
    ADD CONSTRAINT module_name_key UNIQUE (name);


--
-- Name: module_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY module
    ADD CONSTRAINT module_pkey PRIMARY KEY (id);


--
-- Name: oauth_application_perm_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY oauth_application_perm
    ADD CONSTRAINT oauth_application_perm_pkey PRIMARY KEY (application_id, perm);


--
-- Name: oauth_application_registry_ckey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY oauth_application_registry
    ADD CONSTRAINT oauth_application_registry_ckey UNIQUE (consumer_key);


--
-- Name: oauth_application_registry_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY oauth_application_registry
    ADD CONSTRAINT oauth_application_registry_pkey PRIMARY KEY (id);


--
-- Name: oauth_application_token_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY oauth_application_token
    ADD CONSTRAINT oauth_application_token_pkey PRIMARY KEY (id);


--
-- Name: oauth_application_token_token; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY oauth_application_token
    ADD CONSTRAINT oauth_application_token_token UNIQUE (token);


--
-- Name: oauth_nonce_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY oauth_nonce
    ADD CONSTRAINT oauth_nonce_pkey PRIMARY KEY (id);


--
-- Name: oauth_nonce_unique; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY oauth_nonce
    ADD CONSTRAINT oauth_nonce_unique UNIQUE (consumer_key, token, "timestamp", nonce);


--
-- Name: persistent_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY persistent
    ADD CONSTRAINT persistent_pkey PRIMARY KEY (id);


--
-- Name: pivot_task_queue_module_funcion_key_key; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY pivot_task_queue
    ADD CONSTRAINT pivot_task_queue_module_funcion_key_key UNIQUE (module, function, key);


--
-- Name: pivot_task_queue_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY pivot_task_queue
    ADD CONSTRAINT pivot_task_queue_pkey PRIMARY KEY (id);


--
-- Name: predicate_category_key; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY predicate_category
    ADD CONSTRAINT predicate_category_key UNIQUE (predicate_id, is_subject, category_id);


--
-- Name: predicate_category_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY predicate_category
    ADD CONSTRAINT predicate_category_pkey PRIMARY KEY (id);


--
-- Name: protect_id; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY protect
    ADD CONSTRAINT protect_id PRIMARY KEY (id);


--
-- Name: rsc_gone_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY rsc_gone
    ADD CONSTRAINT rsc_gone_pkey PRIMARY KEY (id);


--
-- Name: rsc_name_key; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY rsc
    ADD CONSTRAINT rsc_name_key UNIQUE (name);


--
-- Name: rsc_page_path_key; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY rsc
    ADD CONSTRAINT rsc_page_path_key UNIQUE (page_path);


--
-- Name: rsc_pivot_queue_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY rsc_pivot_queue
    ADD CONSTRAINT rsc_pivot_queue_pkey PRIMARY KEY (rsc_id);


--
-- Name: rsc_pkey; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY rsc
    ADD CONSTRAINT rsc_pkey PRIMARY KEY (id);


--
-- Name: rsc_uri_key; Type: CONSTRAINT; Schema: public; Owner: zotonic; Tablespace: 
--

ALTER TABLE ONLY rsc
    ADD CONSTRAINT rsc_uri_key UNIQUE (uri);


--
-- Name: category_nr_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX category_nr_key ON category USING btree (nr);


--
-- Name: comment_created_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX comment_created_key ON comment USING btree (created);


--
-- Name: comment_rsc_created_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX comment_rsc_created_key ON comment USING btree (rsc_id, created);


--
-- Name: edge_sp_seq_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX edge_sp_seq_key ON edge USING btree (subject_id, predicate_id, seq);


--
-- Name: email_created_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX email_created_key ON emailq USING btree (created);


--
-- Name: email_recipient_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX email_recipient_key ON emailq USING btree (recipient);


--
-- Name: email_status_retry_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX email_status_retry_key ON emailq USING btree (status, retry_on);


--
-- Name: fki_category_parent_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_category_parent_id ON category USING btree (parent_id);


--
-- Name: fki_comment_ip_address; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_comment_ip_address ON comment USING btree (ip_address);


--
-- Name: fki_comment_persistent_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_comment_persistent_id ON comment USING btree (persistent_id);


--
-- Name: fki_comment_rsc_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_comment_rsc_id ON comment USING btree (rsc_id);


--
-- Name: fki_comment_user_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_comment_user_id ON comment USING btree (user_id);


--
-- Name: fki_edge_creator_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_edge_creator_id ON edge USING btree (creator_id);


--
-- Name: fki_edge_object_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_edge_object_id ON edge USING btree (object_id);


--
-- Name: fki_edge_predicate_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_edge_predicate_id ON edge USING btree (predicate_id);


--
-- Name: fki_edge_subject_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_edge_subject_id ON edge USING btree (subject_id);


--
-- Name: fki_identity_rsc_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_identity_rsc_id ON identity USING btree (rsc_id);


--
-- Name: fki_log_rsc_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_log_rsc_id ON log USING btree (rsc_id);


--
-- Name: fki_log_user_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_log_user_id ON log USING btree (user_id);


--
-- Name: fki_predicate_category_category_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_predicate_category_category_id ON predicate_category USING btree (category_id);


--
-- Name: fki_predicate_category_predicate_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_predicate_category_predicate_id ON predicate_category USING btree (predicate_id);


--
-- Name: fki_rsc_created; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_rsc_created ON rsc USING btree (created);


--
-- Name: fki_rsc_creator_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_rsc_creator_id ON rsc USING btree (creator_id);


--
-- Name: fki_rsc_modified; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_rsc_modified ON rsc USING btree (modified);


--
-- Name: fki_rsc_modifier_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_rsc_modifier_id ON rsc USING btree (modifier_id);


--
-- Name: fki_rsc_pivot_queue_due; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_rsc_pivot_queue_due ON rsc_pivot_queue USING btree (is_update, due);


--
-- Name: fki_rsc_pivot_queue_rsc_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX fki_rsc_pivot_queue_rsc_id ON rsc_pivot_queue USING btree (rsc_id);


--
-- Name: identity_created_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX identity_created_key ON identity USING btree (created);


--
-- Name: identity_type_key_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX identity_type_key_key ON identity USING btree (type, key);


--
-- Name: identity_visited_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX identity_visited_key ON identity USING btree (visited);


--
-- Name: log_created_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX log_created_key ON log USING btree (created);


--
-- Name: log_email_content_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX log_email_content_id ON log_email USING btree (content_id, created);


--
-- Name: log_email_created; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX log_email_created ON log_email USING btree (created);


--
-- Name: log_email_envelop_to; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX log_email_envelop_to ON log_email USING btree (envelop_to, created);


--
-- Name: log_email_from_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX log_email_from_id ON log_email USING btree (from_id, created);


--
-- Name: log_email_other_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX log_email_other_id ON log_email USING btree (other_id, created);


--
-- Name: log_email_severity; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX log_email_severity ON log_email USING btree (severity, created);


--
-- Name: log_email_to_id; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX log_email_to_id ON log_email USING btree (to_id, created);


--
-- Name: log_module_created_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX log_module_created_key ON log USING btree (module, created);


--
-- Name: log_type_created_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX log_type_created_key ON log USING btree (type, created);


--
-- Name: medium_deleted_deleted_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX medium_deleted_deleted_key ON medium_deleted USING btree (deleted);


--
-- Name: medium_rootname_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX medium_rootname_key ON medium USING btree (rootname);


--
-- Name: rsc_gone_modified; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_gone_modified ON rsc_gone USING btree (modified);


--
-- Name: rsc_gone_name; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_gone_name ON rsc_gone USING btree (name);


--
-- Name: rsc_gone_page_path; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_gone_page_path ON rsc_gone USING btree (page_path);


--
-- Name: rsc_pivot_category_nr; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_category_nr ON rsc USING btree (pivot_category_nr);


--
-- Name: rsc_pivot_city_street_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_city_street_key ON rsc USING btree (pivot_city, pivot_street);


--
-- Name: rsc_pivot_country_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_country_key ON rsc USING btree (pivot_country);


--
-- Name: rsc_pivot_date_end_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_date_end_key ON rsc USING btree (pivot_date_end);


--
-- Name: rsc_pivot_date_end_month_day_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_date_end_month_day_key ON rsc USING btree (pivot_date_end_month_day);


--
-- Name: rsc_pivot_date_start_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_date_start_key ON rsc USING btree (pivot_date_start);


--
-- Name: rsc_pivot_date_start_month_day_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_date_start_month_day_key ON rsc USING btree (pivot_date_start_month_day);


--
-- Name: rsc_pivot_first_name_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_first_name_key ON rsc USING btree (pivot_first_name);


--
-- Name: rsc_pivot_gender_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_gender_key ON rsc USING btree (pivot_gender);


--
-- Name: rsc_pivot_geocode_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_geocode_key ON rsc USING btree (pivot_geocode);


--
-- Name: rsc_pivot_postcode_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_postcode_key ON rsc USING btree (pivot_postcode);


--
-- Name: rsc_pivot_rtsv_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_rtsv_key ON rsc USING gin (pivot_rtsv);


--
-- Name: rsc_pivot_surname_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_surname_key ON rsc USING btree (pivot_surname);


--
-- Name: rsc_pivot_title_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_title_key ON rsc USING btree (pivot_title);


--
-- Name: rsc_pivot_tsv_key; Type: INDEX; Schema: public; Owner: zotonic; Tablespace: 
--

CREATE INDEX rsc_pivot_tsv_key ON rsc USING gin (pivot_tsv);


--
-- Name: medium_deleted_trigger; Type: TRIGGER; Schema: public; Owner: zotonic
--

CREATE TRIGGER medium_deleted_trigger AFTER DELETE ON medium FOR EACH ROW EXECUTE PROCEDURE medium_delete();


--
-- Name: rsc_update_queue_trigger; Type: TRIGGER; Schema: public; Owner: zotonic
--

CREATE TRIGGER rsc_update_queue_trigger AFTER INSERT OR UPDATE ON rsc FOR EACH ROW EXECUTE PROCEDURE rsc_pivot_update();


--
-- Name: fk_category_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY category
    ADD CONSTRAINT fk_category_id FOREIGN KEY (id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;


--
-- Name: fk_category_parent_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY category
    ADD CONSTRAINT fk_category_parent_id FOREIGN KEY (parent_id) REFERENCES category(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: fk_comment_persistent_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT fk_comment_persistent_id FOREIGN KEY (persistent_id) REFERENCES persistent(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: fk_comment_rsc_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT fk_comment_rsc_id FOREIGN KEY (rsc_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: fk_comment_user_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT fk_comment_user_id FOREIGN KEY (user_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: fk_edge_creator_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY edge
    ADD CONSTRAINT fk_edge_creator_id FOREIGN KEY (creator_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: fk_edge_object_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY edge
    ADD CONSTRAINT fk_edge_object_id FOREIGN KEY (object_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: fk_edge_predicate_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY edge
    ADD CONSTRAINT fk_edge_predicate_id FOREIGN KEY (predicate_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: fk_edge_subject_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY edge
    ADD CONSTRAINT fk_edge_subject_id FOREIGN KEY (subject_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: fk_log_rsc_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY log
    ADD CONSTRAINT fk_log_rsc_id FOREIGN KEY (rsc_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: fk_log_user_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY log
    ADD CONSTRAINT fk_log_user_id FOREIGN KEY (user_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: fk_medium_rsc_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY medium
    ADD CONSTRAINT fk_medium_rsc_id FOREIGN KEY (id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: fk_predicate_category_category_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY predicate_category
    ADD CONSTRAINT fk_predicate_category_category_id FOREIGN KEY (category_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: fk_predicate_category_predicate_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY predicate_category
    ADD CONSTRAINT fk_predicate_category_predicate_id FOREIGN KEY (predicate_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: fk_protect_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY protect
    ADD CONSTRAINT fk_protect_id FOREIGN KEY (id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: fk_rsc_creator_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY rsc
    ADD CONSTRAINT fk_rsc_creator_id FOREIGN KEY (creator_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: fk_rsc_modifier_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY rsc
    ADD CONSTRAINT fk_rsc_modifier_id FOREIGN KEY (modifier_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: fk_rsc_pivot_queue_rsc_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY rsc_pivot_queue
    ADD CONSTRAINT fk_rsc_pivot_queue_rsc_id FOREIGN KEY (rsc_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: oauth_application_appid; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY oauth_application_token
    ADD CONSTRAINT oauth_application_appid FOREIGN KEY (application_id) REFERENCES oauth_application_registry(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: oauth_application_perm_appid; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY oauth_application_perm
    ADD CONSTRAINT oauth_application_perm_appid FOREIGN KEY (application_id) REFERENCES oauth_application_registry(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: oauth_application_registry_user; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY oauth_application_registry
    ADD CONSTRAINT oauth_application_registry_user FOREIGN KEY (user_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: oauth_application_token_user; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY oauth_application_token
    ADD CONSTRAINT oauth_application_token_user FOREIGN KEY (user_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: pk_auth_rsc_id; Type: FK CONSTRAINT; Schema: public; Owner: zotonic
--

ALTER TABLE ONLY identity
    ADD CONSTRAINT pk_auth_rsc_id FOREIGN KEY (rsc_id) REFERENCES rsc(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

