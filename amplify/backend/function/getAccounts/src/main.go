package main

import (
	"context"
	"fmt"
	"log"
	"time"

	"github.com/aws/aws-lambda-go/lambda"
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/guregu/dynamo"
)

type Date struct {
	Year  int `json:"year"`
	Month int `json:"month"`
}

type Account struct {
	ID       int
	Datetime time.Time

	Kind string
	Name string
	User string
}

type Accounts []Account

var (
	sess *session.Session
)

func HandleRequest(ctx context.Context, date Date) (Accounts, error) {
	log.Printf("getAccounts@%d/%d", date.Year, date.Month)
	table := dynamo.New(sess, &aws.Config{Region: aws.String("ap-northeast-1")}).Table("accounts")

	var results Accounts
	table.Scan().Filter("begints_with($, ?)", "time", fmt.Sprintf("%04d/%02d"), date.Year, date.Month).All(&results)
	log.Printf("scan result => %+v", results)

	return results, nil
}

func main() {
	sess = session.New()

	lambda.Start(HandleRequest)
}
