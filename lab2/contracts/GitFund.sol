    // SPDX-License-Identifier: UNLICENSED
    pragma solidity ^0.8.26;

    contract IssueStruct {
        struct Issue {
            uint id;
            uint prize;
            bool merged;     
            string gitlabURL;   
        }

        struct Submission {
            uint subId;
            uint issueId;
            address submitter;
            address[] validators;
            uint validationScore;
            bool validated;
        }

        Issue[] public allIssues;
        Submission[] public submissions;

        function createIssue(string memory gitlabURL) public {
            allIssues.push(Issue(allIssues.length, 0, false, gitlabURL));
        }
        function createSubmission(uint issueId) public {
            require(allIssues[issueId].merged == false, "Issue already merged");
            submissions.push(Submission(submissions.length, issueId, msg.sender, new address[](0), 0, false));
        }
        function getAllIssues() public view returns(Issue[] memory){
            return allIssues;
        }
        function getAllSubmissions() public view returns(Submission[] memory){
            return submissions;
        }
    }

    contract FundManager is IssueStruct {
        event AddedFunds(uint issueId, uint amount);

        function fund(uint index) public payable {
            Issue storage a = allIssues[index];
            require(a.merged == false, "Issue already merged");
            require(msg.value > 0, "Enter a valid amount");
            a.prize += msg.value;

            emit AddedFunds(index, msg.value);
        }
    }

    contract ValidatorMultiSig is IssueStruct {
        event rated(uint submissionIndex, bool accepted);
        event validated(uint submissionIndex);

        function validate(uint submissionIndex, bool accepted) public {
            require(submissionIndex < submissions.length, "Invalid submission index");
            Submission storage s = submissions[submissionIndex];
            require(!s.validated, "Submission already validated");
            require(msg.sender != s.submitter, "Submitter cannot validate");
            for (uint i = 0; i < s.validators.length; i++) {
                require(s.validators[i] != msg.sender, "You have already validated");
            }
            s.validators.push(msg.sender);
            s.validationScore += accepted ? 1 : 0;
            
            if(s.validators.length >= 5 && (s.validationScore * 10 > 8 * s.validators.length)){
            //need 80% of validators to accept and at least 5 validators
                s.validated = true;
                emit validated(submissionIndex);
            }
            emit rated(submissionIndex, accepted);
        }

    }

    contract DeveloperPayout is IssueStruct {
        event readyForMerge(uint issueId);
        function payout(uint submissionIndex) public {
            Submission storage s = submissions[submissionIndex];
            require(s.validated, "Submission not validated");
            Issue storage i = allIssues[s.issueId];
            require(i.merged == false, "Issue already merged");
            payable(s.submitter).transfer(i.prize);

            emit readyForMerge(s.issueId);
            //gitlab api call to merge request
            i.merged = true;
        }
    }
