CREATE TABLE journal_entry (
    id             SERIAL      PRIMARY KEY,
    entry_date     DATE        NOT NULL,
    debit_account  VARCHAR(50) NOT NULL,
    credit_account VARCHAR(50) NOT NULL,
    amount         INTEGER     NOT NULL,
    memo           VARCHAR(100)
);
